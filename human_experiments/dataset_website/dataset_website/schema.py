"""Schema introspection for the public web app.

Postgres is the single source of truth: table/column structure comes from the
catalog via SQLAlchemy reflection, human descriptions come from native COMMENTs,
and the set of public tables is exactly the set `tomcat_public` may SELECT (so
revoking SELECT on an internal table like fnirs_tmp hides it -- no allowlist to
maintain). The only thing not derived from Postgres is which columns are
facetable, which lives in webapp/config.py.

Reflection runs once at startup and is cached; the schema does not change while the
app is running.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from functools import lru_cache
from threading import Lock
from time import monotonic

from sqlalchemy import inspect, text
from sqlalchemy.exc import SQLAlchemyError
from sqlalchemy.types import Boolean, Integer, Numeric

from dataset_website.settings import settings
from dataset_website.config import FACET_COLUMNS
from dataset_website.db import engine

# Numeric SQLAlchemy type bases, used to pick sensible filter operators per column.
_NUMERIC_TYPES = (Integer, Numeric)


@dataclass
class Column:
    name: str
    type_label: str
    nullable: bool
    primary_key: bool
    is_numeric: bool
    facetable: bool = False
    description: str = ""
    foreign_key: bool = False


@dataclass
class ForeignKey:
    """One declared FK constraint: which local columns reference which table."""

    constrained_columns: list[str]
    referred_table: str
    referred_columns: list[str]


@dataclass
class Table:
    name: str
    description: str
    columns: list[Column] = field(default_factory=list)
    primary_keys: list[str] = field(default_factory=list)
    foreign_keys: list[ForeignKey] = field(default_factory=list)

    @property
    def column_names(self) -> list[str]:
        return [c.name for c in self.columns]

    def column(self, name: str) -> Column | None:
        return next((c for c in self.columns if c.name == name), None)

    @property
    def facetable_columns(self) -> list[str]:
        return [c.name for c in self.columns if c.facetable]


@dataclass
class Database:
    name: str
    tables: dict[str, Table] = field(default_factory=dict)


def _is_numeric(col_type) -> bool:
    try:
        return isinstance(col_type, _NUMERIC_TYPES) and not isinstance(
            col_type, Boolean
        )
    except TypeError:
        return False


def _selectable_table_names(conn) -> list[str]:
    """Public tables == base tables in `public` that the current role may SELECT.

    Privilege-driven, not an allowlist: REVOKE SELECT on an internal table to hide
    it. has_table_privilege() is evaluated as the connected role (tomcat_public).
    """
    # NOTE: qualify has_table_privilege() with the row's OWN schema, not a hardcoded
    # 'public.'. The planner may evaluate the privilege predicate before the
    # table_schema filter, so a hardcoded prefix turns a catalog row like
    # pg_catalog.pg_statistic into the non-existent 'public.pg_statistic' and errors.
    # Using %I.%I on (table_schema, table_name) always names a real relation; the
    # table_schema='public' filter then keeps only the public ones.
    rows = (
        conn.execute(
            text(
                """
            SELECT table_name
            FROM information_schema.tables
            WHERE table_schema = 'public'
              AND table_type = 'BASE TABLE'
              AND has_table_privilege(format('%I.%I', table_schema, table_name), 'SELECT')
            ORDER BY table_name
            """
            )
        )
        .scalars()
        .all()
    )
    return list(rows)


@lru_cache(maxsize=1)
def get_database() -> Database:
    inspector = inspect(engine)

    with engine.connect() as conn:
        table_names = _selectable_table_names(conn)

    public = set(table_names)
    tables: dict[str, Table] = {}
    for table_name in table_names:
        facet_cols = set(FACET_COLUMNS.get(table_name, []))
        pk = (
            inspector.get_pk_constraint(table_name).get("constrained_columns", []) or []
        )

        # Declared foreign keys. Drop any whose target isn't itself public, so the
        # exposed graph never names a table tomcat_public can't SELECT (fail-closed).
        foreign_keys: list[ForeignKey] = []
        fk_cols: set[str] = set()
        for fk in inspector.get_foreign_keys(table_name):
            referred = fk.get("referred_table")
            if not referred or referred not in public:
                continue
            cols = list(fk.get("constrained_columns", []) or [])
            foreign_keys.append(
                ForeignKey(
                    constrained_columns=cols,
                    referred_table=referred,
                    referred_columns=list(fk.get("referred_columns", []) or []),
                )
            )
            fk_cols.update(cols)

        try:
            table_comment = (inspector.get_table_comment(table_name) or {}).get(
                "text"
            ) or ""
        except NotImplementedError:
            table_comment = ""

        columns: list[Column] = []
        for col in inspector.get_columns(table_name):
            name = col["name"]
            col_type = col["type"]
            columns.append(
                Column(
                    name=name,
                    type_label=str(col_type),
                    nullable=bool(col.get("nullable", True)),
                    primary_key=name in pk,
                    is_numeric=_is_numeric(col_type),
                    facetable=name in facet_cols,
                    # Native per-column COMMENT, read straight from the catalog.
                    description=col.get("comment") or "",
                    foreign_key=name in fk_cols,
                )
            )

        tables[table_name] = Table(
            name=table_name,
            description=table_comment,
            columns=columns,
            primary_keys=list(pk),
            foreign_keys=foreign_keys,
        )

    return Database(name=settings.db_name, tables=tables)


# --- Estimated row counts ---------------------------------------------------
# Held in memory rather than read per request. reltuples only moves on ingestion,
# so a stale estimate costs nothing; a fresh one used to cost a great deal. The
# index pages render ~24 tables and called this once per table, each opening its
# own connection -- so whenever Postgres was slow, `/` serialised up to 24 doomed
# round-trips, each able to burn the full db.STATEMENT_TIMEOUT_MS before erroring.
# That is exactly how the 2026-09-05/06 outages turned a memory-starved host into
# a 500 on the homepage (the queries themselves are catalog lookups, microseconds
# when the page cache is warm). One connection, one query, cached, and stale on
# failure instead of raising.
ROW_COUNT_TTL_SECONDS = 900
# After a failed refresh, serve the previous snapshot for this long rather than
# retrying on every request -- a starved database must not be re-probed 24 times
# a page.
ROW_COUNT_RETRY_SECONDS = 60

_row_counts: dict[str, int] = {}
_row_counts_expires_at: float = 0.0
_row_counts_lock = Lock()


def _fetch_row_counts() -> dict[str, int]:
    """Every public table's reltuples in a single catalog query.

    Restricted to ordinary and partitioned tables visible on the search path, so a
    same-named index or a relation in another schema can't shadow the real entry
    (the per-table query this replaced matched on relname alone).
    """
    names = list(get_database().tables)
    if not names:
        return {}
    with engine.connect() as conn:
        rows = conn.execute(
            text(
                "SELECT relname, reltuples::bigint FROM pg_class "
                "WHERE relname = ANY(:names) "
                "AND relkind IN ('r', 'p') "
                "AND pg_table_is_visible(oid)"
            ),
            {"names": names},
        ).all()
    return {name: int(count) for name, count in rows if count and count > 0}


def row_counts(*, force: bool = False) -> dict[str, int]:
    """Cached {table_name: estimated rows}, refreshed at most once per TTL.

    Never raises on a database failure: the previous snapshot is served instead,
    and an empty one on the very first failure. Callers then see 0, which is
    already the "unknown" value here (reltuples is -1 before a first ANALYZE).
    """
    global _row_counts, _row_counts_expires_at

    with _row_counts_lock:
        # Re-checked under the lock, so concurrent requests share one refresh
        # instead of each firing its own query at an already-struggling database.
        if not force and monotonic() < _row_counts_expires_at:
            return _row_counts
        try:
            _row_counts = _fetch_row_counts()
            _row_counts_expires_at = monotonic() + ROW_COUNT_TTL_SECONDS
        except SQLAlchemyError:
            # Degrade, don't 500. The expiry is still advanced so the retry is
            # bounded; waiters that wake after this see it and return immediately.
            _row_counts_expires_at = monotonic() + ROW_COUNT_RETRY_SECONDS
        return _row_counts


def estimated_row_count(table_name: str) -> int:
    """Fast, approximate row count from pg_class.reltuples.

    reltuples is an estimate maintained by ANALYZE/autovacuum, so the table index
    page never blocks on COUNT(*) over millions of rows -- the very thing that makes
    the current Datasette index slow. Returns 0 when unknown (reltuples is -1 before
    a table is first analyzed, and after a failed refresh).
    """
    return row_counts().get(table_name, 0)
