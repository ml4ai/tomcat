#!/usr/bin/env python
"""Import table/column descriptions from metadata.yml into native Postgres COMMENTs.

This is the bridge that makes Postgres self-describing: the curated prose that
historically lived in metadata.yml (and fed the Datasette UI) is written as
COMMENT ON TABLE / COMMENT ON COLUMN, where the new web app reads it back via
reflection. Idempotent -- COMMENT overwrites -- so it is safe to re-run whenever
metadata.yml changes, and it can double as the authoring workflow (edit YAML, sync)
if the team prefers that over editing comments directly.

Run with the read-WRITE pipeline connection (a tomcat_curators member owns the
tables and may comment on them):

    working_env=production db_pass=<pass> PYTHONPATH="." ./bin/sync_table_comments.py
"""

from pathlib import Path

import yaml
from psycopg2.extensions import quote_ident

from dataset_pipeline.common.config import settings
from dataset_pipeline.database.config import engine

# metadata.yml lives with the website project (it is also that site's page
# chrome); this script stays pipeline-side because it needs the read-write
# engine. The two projects are siblings under human_experiments/.
PROJECT_ROOT = Path(__file__).resolve().parents[1]
METADATA_PATH = PROJECT_ROOT.parent / "dataset_website" / "metadata.yml"


def _existing(cur, kind: str, table: str, column: str | None = None) -> bool:
    """Whether a table (or column) currently exists, to skip stale metadata."""
    if kind == "table":
        cur.execute(
            "SELECT 1 FROM information_schema.tables "
            "WHERE table_schema = 'public' AND table_name = %s",
            (table,),
        )
    else:
        cur.execute(
            "SELECT 1 FROM information_schema.columns "
            "WHERE table_schema = 'public' AND table_name = %s AND column_name = %s",
            (table, column),
        )
    return cur.fetchone() is not None


def main() -> None:
    with open(METADATA_PATH) as f:
        metadata = yaml.safe_load(f) or {}

    db_meta = metadata.get("databases", {}).get(settings.db_name, {})
    table_meta = db_meta.get("tables", {})

    raw = engine.raw_connection()
    try:
        cur = raw.cursor()
        n_tables = n_columns = 0
        for table_name, tmeta in table_meta.items():
            tmeta = tmeta or {}
            if not _existing(cur, "table", table_name):
                print(f"  skip (no such table): {table_name}")
                continue

            ident = quote_ident(table_name, cur)
            description = tmeta.get("description")
            if description:
                # psycopg2 binds %s client-side into a quoted literal, producing
                # valid DDL; the identifier is quoted via quote_ident.
                cur.execute(
                    f"COMMENT ON TABLE public.{ident} IS %s", (description.strip(),)
                )
                n_tables += 1

            for col_name, col_desc in (tmeta.get("columns") or {}).items():
                if not col_desc:
                    continue
                if not _existing(cur, "column", table_name, col_name):
                    print(f"  skip (no such column): {table_name}.{col_name}")
                    continue
                col_ident = quote_ident(col_name, cur)
                cur.execute(
                    f"COMMENT ON COLUMN public.{ident}.{col_ident} IS %s",
                    (str(col_desc).strip(),),
                )
                n_columns += 1

        raw.commit()
        print(f"Applied comments: {n_tables} tables, {n_columns} columns.")
    finally:
        raw.close()


if __name__ == "__main__":
    main()
