"""Table browse engine: URL query params -> parameterized SQLAlchemy Core SELECT.

We preserve Datasette's compact URL grammar so existing deep links and saved URLs
keep working:

    /tomcat/eeg_raw?station__exact=lion&id__gt=100&_sort_desc=id&_size=50&_offset=50

- `col=value`          -> col = value            (exact, the bare form)
- `col__<op>=value`    -> the operator in OPERATORS below
- `col__isnull=1`      -> value-less operators (IS NULL / IS NOT NULL)
- `_sort=col` / `_sort_desc=col`                 sort ascending / descending
- `_size` / `_offset`                            pagination (offset-based)

Every column name is validated against the reflected schema and every value is a
bound parameter -- the only SQL text we ever interpolate is a known-good column
name. Combined with the read-only `tomcat_public` role and its statement timeout,
there is no way to reach a write or an injection here.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from sqlalchemy import Integer, and_, asc, column, desc, func, select, table
from sqlalchemy.sql.elements import ColumnClause

from dataset_website.config import DEFAULT_PAGE_SIZE, MAX_PAGE_SIZE
from dataset_website.db import engine
from dataset_website.schema import Table


# (label, needs_value): how each operator renders in the UI and whether it takes
# a right-hand value. The callable builds the SQLAlchemy condition.
@dataclass(frozen=True)
class Operator:
    key: str
    label: str
    needs_value: bool
    build: object  # (ColumnClause, value) -> condition


def _contains(c, v):
    return c.ilike(f"%{v}%")


def _startswith(c, v):
    return c.ilike(f"{v}%")


def _endswith(c, v):
    return c.ilike(f"%{v}")


OPERATORS: dict[str, Operator] = {
    "exact": Operator("exact", "=", True, lambda c, v: c == v),
    "not": Operator("not", "≠", True, lambda c, v: c != v),
    "gt": Operator("gt", ">", True, lambda c, v: c > v),
    "gte": Operator("gte", "≥", True, lambda c, v: c >= v),
    "lt": Operator("lt", "<", True, lambda c, v: c < v),
    "lte": Operator("lte", "≤", True, lambda c, v: c <= v),
    "contains": Operator("contains", "contains", True, _contains),
    "startswith": Operator("startswith", "starts with", True, _startswith),
    "endswith": Operator("endswith", "ends with", True, _endswith),
    "isnull": Operator("isnull", "is null", False, lambda c, v: c.is_(None)),
    "notnull": Operator("notnull", "is not null", False, lambda c, v: c.isnot(None)),
}

# Querystring keys the browse engine interprets itself rather than as filters.
RESERVED_PARAMS = {
    "_sort",
    "_sort_desc",
    "_size",
    "_offset",
    "_next",
    "_search",
    "_format",
}


@dataclass
class AppliedFilter:
    column: str
    op: str
    value: str | None
    label: str  # human-readable, e.g. "station = lion"


@dataclass
class BrowseResult:
    table: Table
    column_names: list[str]
    rows: list[dict]
    filters: list[AppliedFilter]
    sort: str | None
    sort_desc: str | None
    size: int
    offset: int
    has_next: bool
    estimated_total: int = 0
    extra: dict = field(default_factory=dict)


def _coerce(table_obj: Table, col_name: str, value: str):
    """Send numeric columns a typed param so Postgres comparisons are well-typed."""
    col = table_obj.column(col_name)
    if col and col.is_numeric and value is not None:
        try:
            return int(value)
        except ValueError:
            try:
                return float(value)
            except ValueError:
                return value
    return value


def parse_filters(
    table_obj: Table, params: list[tuple[str, str]]
) -> list[AppliedFilter]:
    """Extract validated filters from (key, value) querystring pairs.

    Unknown columns or operators are ignored rather than erroring, matching
    Datasette's forgiving behaviour with hand-edited URLs.
    """
    valid_columns = set(table_obj.column_names)
    filters: list[AppliedFilter] = []
    for key, value in params:
        if key in RESERVED_PARAMS or key.startswith("_"):
            continue
        if "__" in key:
            col_name, op_key = key.rsplit("__", 1)
        else:
            col_name, op_key = key, "exact"
        if col_name not in valid_columns or op_key not in OPERATORS:
            continue
        op = OPERATORS[op_key]
        if op.needs_value:
            filters.append(
                AppliedFilter(col_name, op_key, value, f"{col_name} {op.label} {value}")
            )
        else:
            filters.append(
                AppliedFilter(col_name, op_key, None, f"{col_name} {op.label}")
            )
    return filters


def _condition(table_obj: Table, sa_columns: dict[str, ColumnClause], f: AppliedFilter):
    op = OPERATORS[f.op]
    col = sa_columns[f.column]
    value = _coerce(table_obj, f.column, f.value) if op.needs_value else None
    return op.build(col, value)


def compile_filters(
    table_obj: Table, params: list[tuple[str, str]]
) -> tuple[dict[str, ColumnClause], list[AppliedFilter], list]:
    """Shared by browse, count, and facets: build columns + WHERE from the URL.

    Returns (sa_columns, filters, where_clauses) so callers can compose their own
    SELECT against the same validated filter set.
    """
    sa_columns = {c.name: column(c.name) for c in table_obj.columns}
    filters = parse_filters(table_obj, params)
    where = [_condition(table_obj, sa_columns, f) for f in filters]
    return sa_columns, filters, where


def run_browse(
    table_obj: Table,
    params: list[tuple[str, str]],
    *,
    estimated_total: int = 0,
    limit_override: int | None = None,
) -> BrowseResult:
    """Execute a browse query and return rows + UI state.

    `limit_override` is used by the CSV/JSON exporters to stream more rows than the
    HTML page size (still bounded by the role's statement timeout).
    """
    sa_columns, filters, where = compile_filters(table_obj, params)
    tbl = table(table_obj.name, *sa_columns.values())

    # Pagination.
    qs = dict(params)
    size = min(
        int(qs.get("_size", DEFAULT_PAGE_SIZE) or DEFAULT_PAGE_SIZE), MAX_PAGE_SIZE
    )
    size = max(size, 1)
    offset = max(int(qs.get("_offset", 0) or 0), 0)

    # Sort. Default to primary key for stable, index-friendly ordering.
    sort = qs.get("_sort")
    sort_desc = qs.get("_sort_desc")
    valid = set(table_obj.column_names)
    order_by = []
    if sort_desc in valid:
        order_by = [desc(sa_columns[sort_desc])]
    elif sort in valid:
        order_by = [asc(sa_columns[sort])]
    elif table_obj.primary_keys:
        order_by = [
            asc(sa_columns[pk]) for pk in table_obj.primary_keys if pk in sa_columns
        ]

    stmt = select(*sa_columns.values())
    if where:
        stmt = stmt.where(and_(*where))
    if order_by:
        stmt = stmt.order_by(*order_by)

    effective_limit = limit_override if limit_override is not None else size
    # Fetch one extra row to know whether a next page exists without a COUNT.
    stmt = stmt.select_from(tbl).offset(offset).limit(effective_limit + 1)

    with engine.connect() as conn:
        result = conn.execute(stmt)
        col_names = list(result.keys())
        fetched = [dict(row) for row in result.mappings()]

    has_next = len(fetched) > effective_limit
    rows = fetched[:effective_limit]

    return BrowseResult(
        table=table_obj,
        column_names=col_names,
        rows=rows,
        filters=filters,
        sort=sort if sort in valid else None,
        sort_desc=sort_desc if sort_desc in valid else None,
        size=size,
        offset=offset,
        has_next=has_next,
        estimated_total=estimated_total,
    )


def count_rows(table_obj: Table, params: list[tuple[str, str]]) -> int:
    """Exact COUNT(*) for the current filter set.

    Only safe to call on a filtered/bounded query -- the statement timeout protects
    against an unfiltered COUNT over a huge table. The table index page uses the
    pg_class estimate instead (see schema.estimated_row_count).
    """
    sa_columns, _filters, where = compile_filters(table_obj, params)
    tbl = table(table_obj.name, *sa_columns.values())

    stmt = select(func.count().cast(Integer)).select_from(tbl)
    if where:
        stmt = stmt.where(and_(*where))
    with engine.connect() as conn:
        return int(conn.execute(stmt).scalar() or 0)
