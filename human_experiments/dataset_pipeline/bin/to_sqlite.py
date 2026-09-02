#!/usr/bin/env python
"""Export Postgres tables into a single SQLite bulk-download artifact.

Streams every table server-side with ``COPY ... TO STDOUT`` and loads it into
SQLite in bounded batches, so peak memory is a few hundred MB regardless of
table size. (The previous ``db-to-sqlite`` invocation ran ``select *`` through a
client-side-buffered cursor, which reached 94 GB RSS on ``eeg_sync`` before
writing a row.)

By default the seven high-rate signal tables are left out: together they are
~400 GB of the 413 GB database, SQLite does not compress, and a single-file
artifact of that size is neither downloadable nor usable in pandas/R/DuckDB.
Those tables are in the pg_dump artifact (``make pg_dump_artifact``) and on the
live site. Pass ``--include all`` to override.

The load writes to ``<output>.partial`` and records each finished table in a
``_export_progress`` table, so a killed run resumes where it stopped instead of
starting over. Only the final rename produces a file the web app will serve.
"""

from __future__ import annotations

import argparse
import hashlib
import logging
import os
import re
import sqlite3
import sys
import time
from datetime import datetime, timezone

import psycopg2

from dataset_pipeline.common.config import (
    LOG_DIR,
    PRODUCTION,
    TMP_DIR,
    configure_logging,
    settings,
)

TABLES = {
    "affective_task_event",
    "audio_vocalics",
    "data_validity",
    "eeg_device",
    "eeg_raw",
    "eeg_sync",
    "ekg_sync",
    "finger_tapping_task_observation",
    "fnirs_raw",
    "fnirs_sync",
    "gaze_raw",
    "group_session",
    "gsr_sync",
    "minecraft_mission",
    "minecraft_testbed_message",
    "modality",
    "participant",
    "ping_pong_competitive_task_observation",
    "ping_pong_cooperative_task_observation",
    "post_game_survey",
    "rest_state_task",
    "screen_capture",
    "station",
    "task",
}

# Excluded by default; see the module docstring. Sizes measured 2026-08-31.
SIGNAL_TABLES = {
    "eeg_raw",  # 181 GB
    "gaze_raw",  # 47 GB
    "fnirs_sync",  # 47 GB
    "eeg_sync",  # 37 GB
    "audio_vocalics",  # 34 GB
    "gsr_sync",  # 27 GB
    "ekg_sync",  # 23 GB
}
CORE_TABLES = TABLES - SIGNAL_TABLES

PROGRESS_TABLE = "_export_progress"
DOCS_TABLE = "_schema_docs"

# Postgres udt_name -> SQLite declared type. Everything else becomes TEXT, which
# is also what COPY's text format hands us for json/jsonb/timestamps.
SQLITE_TYPES = {
    "int2": "INTEGER",
    "int4": "INTEGER",
    "int8": "INTEGER",
    "bool": "INTEGER",
    "float4": "REAL",
    "float8": "REAL",
    "numeric": "REAL",
    "bytea": "BLOB",
}

_COPY_ESCAPES = {
    "b": "\b",
    "f": "\f",
    "n": "\n",
    "r": "\r",
    "t": "\t",
    "v": "\v",
    "\\": "\\",
}
_COPY_ESCAPE_RE = re.compile(r"\\(.)")


def _unescape(value: str) -> str:
    if "\\" not in value:
        return value
    return _COPY_ESCAPE_RE.sub(
        lambda m: _COPY_ESCAPES.get(m.group(1), m.group(1)), value
    )


def _converter(udt_name: str):
    """Return the str -> Python value function for one column."""
    if udt_name in ("int2", "int4", "int8"):
        return int
    if udt_name in ("float4", "float8", "numeric"):
        return float
    if udt_name == "bool":
        return lambda v: 1 if v == "t" else 0
    if udt_name == "bytea":
        # COPY text emits bytea as \x-prefixed hex.
        return lambda v: bytes.fromhex(v[2:]) if v.startswith("\\x") else v.encode()
    return _unescape


def q(identifier: str) -> str:
    """Double-quote an identifier for both Postgres and SQLite."""
    return '"' + identifier.replace('"', '""') + '"'


class BatchLoader:
    """File-like sink for ``copy_expert``: parses COPY text rows and inserts
    them into SQLite in batches bounded by row count and byte size."""

    def __init__(self, sqlite_conn, insert_sql, converters, batch_rows, batch_bytes):
        self.conn = sqlite_conn
        self.insert_sql = insert_sql
        self.converters = converters
        self.batch_rows = batch_rows
        self.batch_bytes = batch_bytes
        self.buffer = bytearray()
        self.rows = []
        self.pending_bytes = 0
        self.total_rows = 0

    def write(self, data):
        if isinstance(data, str):
            data = data.encode("utf-8")
        self.buffer += data
        start = 0
        while True:
            end = self.buffer.find(b"\n", start)
            if end < 0:
                break
            self._add_line(self.buffer[start:end])
            start = end + 1
        del self.buffer[:start]
        if len(self.rows) >= self.batch_rows or self.pending_bytes >= self.batch_bytes:
            self.flush()

    def _add_line(self, raw: bytes):
        fields = raw.decode("utf-8").split("\t")
        row = tuple(
            None if f == "\\N" else conv(f) for f, conv in zip(fields, self.converters)
        )
        self.rows.append(row)
        self.pending_bytes += len(raw)

    def flush(self):
        if self.rows:
            self.conn.executemany(self.insert_sql, self.rows)
            self.conn.commit()
            self.total_rows += len(self.rows)
            self.rows = []
            self.pending_bytes = 0

    def close(self):
        if self.buffer:
            self._add_line(bytes(self.buffer))
            self.buffer.clear()
        self.flush()


def pg_connection():
    kwargs = {
        "dbname": settings.db_name,
        "user": settings.db_user,
        "host": settings.db_host,
        "port": settings.db_port,
    }
    if not settings.db_host.startswith("/"):
        kwargs["password"] = settings.db_pass
    conn = psycopg2.connect(**kwargs)
    # One snapshot for the whole export: the COPY and the row count it is
    # checked against see the same data.
    conn.set_session(isolation_level="REPEATABLE READ", readonly=True)
    return conn


def table_columns(pg, table):
    with pg.cursor() as cur:
        cur.execute(
            """
            select column_name, udt_name, is_nullable = 'NO'
            from information_schema.columns
            where table_schema = 'public' and table_name = %s
            order by ordinal_position
            """,
            (table,),
        )
        return cur.fetchall()


def table_indexes(pg, table):
    """[(index_name, unique, [columns])] for the table's btree indexes."""
    with pg.cursor() as cur:
        cur.execute(
            """
            select i.relname, ix.indisunique,
                   array_agg(a.attname order by k.ord)
            from pg_index ix
            join pg_class t on t.oid = ix.indrelid
            join pg_class i on i.oid = ix.indexrelid
            join pg_namespace n on n.oid = t.relnamespace
            cross join lateral unnest(ix.indkey) with ordinality as k(attnum, ord)
            join pg_attribute a on a.attrelid = t.oid and a.attnum = k.attnum
            where n.nspname = 'public' and t.relname = %s
            group by i.relname, ix.indisunique, ix.indisprimary
            order by ix.indisprimary desc, i.relname
            """,
            (table,),
        )
        return cur.fetchall()


def primary_key(pg, table):
    with pg.cursor() as cur:
        cur.execute(
            """
            select array_agg(a.attname order by k.ord)
            from pg_index ix
            join pg_class t on t.oid = ix.indrelid
            join pg_namespace n on n.oid = t.relnamespace
            cross join lateral unnest(ix.indkey) with ordinality as k(attnum, ord)
            join pg_attribute a on a.attrelid = t.oid and a.attnum = k.attnum
            where n.nspname = 'public' and t.relname = %s and ix.indisprimary
            """,
            (table,),
        )
        row = cur.fetchone()
        return row[0] if row and row[0] else []


def row_count(pg, table):
    with pg.cursor() as cur:
        cur.execute(f"select count(*) from {q(table)}")
        return cur.fetchone()[0]


def schema_docs(pg, tables):
    """(table, column-or-NULL, description) from Postgres COMMENTs."""
    with pg.cursor() as cur:
        cur.execute(
            """
            select c.relname, a.attname, d.description
            from pg_description d
            join pg_class c on c.oid = d.objoid
            join pg_namespace n on n.oid = c.relnamespace
            left join pg_attribute a
                   on a.attrelid = c.oid and a.attnum = d.objsubid and d.objsubid > 0
            where n.nspname = 'public' and c.relkind = 'r'
              and c.relname = any(%s)
            order by c.relname, a.attnum nulls first
            """,
            (list(tables),),
        )
        return cur.fetchall()


def export_table(pg, lite, table, batch_rows, batch_bytes):
    log = logging.getLogger("to_sqlite")
    columns = table_columns(pg, table)
    if not columns:
        raise RuntimeError(f"{table}: no such table in Postgres")
    pk = primary_key(pg, table)

    # Drop whatever a killed run left behind for this table and start it fresh.
    lite.execute(f"drop table if exists {q(table)}")
    ddl_cols = ", ".join(
        f"{q(name)} {SQLITE_TYPES.get(udt, 'TEXT')}{' NOT NULL' if notnull else ''}"
        for name, udt, notnull in columns
    )
    lite.execute(f"create table {q(table)} ({ddl_cols})")
    lite.commit()

    col_list = ", ".join(q(name) for name, _, _ in columns)
    order = f" order by {', '.join(q(c) for c in pk)}" if pk else ""
    copy_sql = f"copy (select {col_list} from {q(table)}{order}) to stdout"
    insert_sql = (
        f"insert into {q(table)} ({col_list}) values "
        f"({', '.join('?' for _ in columns)})"
    )
    converters = [_converter(udt) for _, udt, _ in columns]

    expected = row_count(pg, table)
    log.info("%s: %s rows, %d columns", table, f"{expected:,}", len(columns))
    started = time.monotonic()
    loader = BatchLoader(lite, insert_sql, converters, batch_rows, batch_bytes)
    with pg.cursor() as cur:
        cur.copy_expert(copy_sql, loader)
    loader.close()
    elapsed = time.monotonic() - started
    if loader.total_rows != expected:
        raise RuntimeError(
            f"{table}: loaded {loader.total_rows:,} rows but Postgres has {expected:,}"
        )
    log.info(
        "%s: loaded in %.0fs (%.0f rows/s)",
        table,
        elapsed,
        loader.total_rows / elapsed if elapsed else 0,
    )

    # Indexes after the load: a sort-based build is far cheaper than maintaining
    # a b-tree across millions of inserts. The primary key becomes a UNIQUE
    # index of the same name.
    indexes = table_indexes(pg, table)
    for name, unique, cols in indexes:
        cols_sql = ", ".join(q(c) for c in cols)
        lite.execute(
            f"create {'unique ' if unique else ''}index {q(name)} "
            f"on {q(table)} ({cols_sql})"
        )
    lite.commit()
    log.info("%s: %d index(es) built", table, len(indexes))

    lite.execute(
        f"insert or replace into {q(PROGRESS_TABLE)} (table_name, rows, finished_at) "
        "values (?, ?, ?)",
        (table, loader.total_rows, datetime.now(timezone.utc).isoformat()),
    )
    lite.commit()


def write_docs(pg, lite, tables):
    lite.execute(f"drop table if exists {q(DOCS_TABLE)}")
    lite.execute(
        f"create table {q(DOCS_TABLE)} "
        "(table_name TEXT NOT NULL, column_name TEXT, description TEXT NOT NULL)"
    )
    rows = schema_docs(pg, tables)
    lite.executemany(f"insert into {q(DOCS_TABLE)} values (?, ?, ?)", rows)
    lite.commit()
    logging.getLogger("to_sqlite").info(
        "%s: %d table/column descriptions", DOCS_TABLE, len(rows)
    )


def sha256_file(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def parse_tables(spec, what):
    names = [t.strip() for t in spec.split(",") if t.strip()]
    unknown = sorted(set(names) - TABLES)
    if unknown:
        raise SystemExit(f"{what}: unknown table(s) {', '.join(unknown)}")
    return set(names)


def main():
    parser = argparse.ArgumentParser(
        description="Export Postgres tables to a single SQLite database, streaming."
    )
    parser.add_argument(
        "--include",
        default=os.getenv("TBS", "core"),
        help="'core' (default: every table except the seven high-rate signal "
        "tables), 'all', or a comma-separated list of tables. Env: TBS.",
    )
    parser.add_argument(
        "--exclude", default="", help="Comma-separated tables to drop from --include."
    )
    parser.add_argument(
        "--output",
        default=None,
        help="Output path (default: <artifact_dir>/tomcat-core.db, or "
        "<artifact_dir>/tomcat.db with --include all).",
    )
    parser.add_argument("--batch-rows", type=int, default=20_000)
    parser.add_argument("--batch-mb", type=int, default=32)
    parser.add_argument(
        "--dry-run", action="store_true", help="Print the plan and exit."
    )
    args = parser.parse_args()

    if settings.working_env != PRODUCTION:
        raise SystemExit(
            "to_sqlite reads Postgres; run with working_env=production (the "
            "Makefile target does this)."
        )

    spec = args.include.strip()
    if spec == "core":
        tables = set(CORE_TABLES)
    elif spec == "all":
        tables = set(TABLES)
    else:
        tables = parse_tables(spec, "--include")
    if args.exclude:
        tables -= parse_tables(args.exclude, "--exclude")
    tables = sorted(tables)

    output = args.output or (
        f"{settings.artifact_dir}/tomcat.db"
        if spec == "all"
        else f"{settings.artifact_dir}/tomcat-core.db"
    )
    partial = output + ".partial"

    signal_included = sorted(set(tables) & SIGNAL_TABLES)
    print(f"Output:  {output}")
    print(f"Tables:  {', '.join(tables)}")
    if signal_included:
        print(
            f"Warning: includes signal tables {', '.join(signal_included)}; "
            "expect hundreds of GB and many hours."
        )
    if args.dry_run:
        return

    configure_logging(f"{LOG_DIR}/to_sqlite.log")
    log = logging.getLogger("to_sqlite")
    # Index builds spill to temp files; keep them on the artifact disk.
    os.environ["SQLITE_TMPDIR"] = TMP_DIR

    pg = pg_connection()
    lite = sqlite3.connect(partial)
    lite.execute("pragma journal_mode = off")
    lite.execute("pragma synchronous = off")
    lite.execute("pragma cache_size = -262144")  # 256 MB
    lite.execute(
        f"create table if not exists {q(PROGRESS_TABLE)} "
        "(table_name TEXT PRIMARY KEY, rows INTEGER NOT NULL, finished_at TEXT NOT NULL)"
    )
    done = {r[0] for r in lite.execute(f"select table_name from {q(PROGRESS_TABLE)}")}
    if done:
        log.info("resuming %s: %d table(s) already complete", partial, len(done))

    started = time.monotonic()
    for table in tables:
        if table in done:
            log.info("%s: already exported, skipping", table)
            continue
        export_table(pg, lite, table, args.batch_rows, args.batch_mb << 20)

    write_docs(pg, lite, tables)
    lite.execute(f"drop table {q(PROGRESS_TABLE)}")
    lite.commit()
    lite.execute("pragma journal_mode = delete")
    lite.close()
    pg.close()

    os.replace(partial, output)
    digest = sha256_file(output)
    with open(output + ".sha256", "w") as fh:
        fh.write(f"{digest}  {os.path.basename(output)}\n")
    size_gb = os.path.getsize(output) / 1e9
    log.info(
        "done: %s (%.2f GB, sha256 %s) in %.0f min",
        output,
        size_gb,
        digest,
        (time.monotonic() - started) / 60,
    )


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        sys.exit(130)
