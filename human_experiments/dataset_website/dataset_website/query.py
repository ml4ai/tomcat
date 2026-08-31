"""Arbitrary read-only SQL console.

The hard security guarantee comes from Postgres, not from this file: queries run as
`tomcat_public`, a role that can only SELECT, inside a read-only transaction, under
a 10s statement timeout. So even a query that slips past the checks below cannot
write, cannot read another database, and cannot run away.

The app-layer guard here is *defense-in-depth plus good UX*: it rejects anything
that isn't a single SELECT/WITH statement up front, with a clear message, rather
than letting a confusing permission error surface from the driver. We parse with
sqlparse rather than regex so string literals and comments containing keywords or
semicolons don't fool the check.
"""

from __future__ import annotations

from dataclasses import dataclass

import sqlparse
from sqlalchemy import text
from sqlalchemy.exc import SQLAlchemyError

from dataset_website.config import SQL_CONSOLE_ROW_CAP
from dataset_website.db import engine

# Leading keywords we accept. Everything else (INSERT/UPDATE/DELETE/DDL/SET/COPY/…)
# is refused before it ever reaches the database.
ALLOWED_LEADING = {"SELECT", "WITH"}


class InvalidQuery(ValueError):
    """Raised when a console query fails the SELECT-only guard."""


@dataclass
class QueryResult:
    columns: list[str]
    rows: list[dict]
    truncated: bool
    row_count: int


def validate(sql: str) -> str:
    """Return the cleaned SQL if it is a single read-only statement, else raise.

    Raises InvalidQuery with a human-readable reason.
    """
    if not sql or not sql.strip():
        raise InvalidQuery("Enter a SQL query.")

    statements = [s for s in sqlparse.parse(sql) if str(s).strip().strip(";")]
    if len(statements) == 0:
        raise InvalidQuery("Enter a SQL query.")
    if len(statements) > 1:
        raise InvalidQuery("Only a single statement is allowed (no semicolons).")

    stmt = statements[0]
    first = stmt.token_first(skip_cm=True)  # skip leading comments/whitespace
    if first is None:
        raise InvalidQuery("Enter a SQL query.")
    if first.normalized.upper() not in ALLOWED_LEADING:
        raise InvalidQuery(
            "Only read-only SELECT (or WITH … SELECT) queries are allowed."
        )

    return sql.strip().rstrip(";").strip()


def run_query(sql: str, *, cap: int = SQL_CONSOLE_ROW_CAP) -> QueryResult:
    """Validate, bound, and execute a console query.

    The user SQL is wrapped in an outer SELECT … LIMIT so a bare
    `SELECT * FROM eeg_raw` can't try to materialise millions of rows. We fetch one
    extra row to report truncation. Driver/permission errors are surfaced as
    InvalidQuery so the UI can show them inline.
    """
    cleaned = validate(sql)

    # Wrap to enforce a row cap regardless of the user's own LIMIT. Bound param for
    # the cap; the inner SQL is intentionally user-controlled (that is the feature),
    # and is harmless because the connection role can only read under a timeout.
    wrapped = text(f"SELECT * FROM (\n{cleaned}\n) AS _tomcat_sub LIMIT :_cap")

    try:
        with engine.connect() as conn:
            result = conn.execute(wrapped, {"_cap": cap + 1})
            columns = list(result.keys())
            fetched = [dict(row) for row in result.mappings()]
    except SQLAlchemyError as exc:
        # e.g. permission denied, statement timeout, syntax error, unknown column.
        raise InvalidQuery(_clean_db_error(exc)) from exc

    truncated = len(fetched) > cap
    rows = fetched[:cap]
    return QueryResult(
        columns=columns, rows=rows, truncated=truncated, row_count=len(rows)
    )


def _clean_db_error(exc: SQLAlchemyError) -> str:
    """Surface the Postgres message without the SQLAlchemy wrapper noise."""
    orig = getattr(exc, "orig", None)
    msg = str(orig) if orig is not None else str(exc)
    return msg.strip().splitlines()[0] if msg.strip() else "Query failed."
