"""Opt-in faceting: value distributions for hand-picked low-cardinality columns.

Faceting is the single most expensive thing the old Datasette site did on the wide
signal tables, so here it is strictly opt-in: only columns listed in
config.FACET_COLUMNS are ever faceted, and each facet query runs under the same 10s
statement timeout as everything else. If a facet times out (or otherwise errors) we
drop just that facet and mark it, rather than failing the page -- mirroring
Datasette's "these facets timed out" behaviour.

Facets respect the currently-applied filters, so they describe the filtered view.
"""

from __future__ import annotations

from dataclasses import dataclass, field

from sqlalchemy import and_, desc, func, select, table
from sqlalchemy.exc import SQLAlchemyError

from dataset_website.browse import compile_filters
from dataset_website.config import FACET_SIZE
from dataset_website.db import engine
from dataset_website.schema import Table


@dataclass
class FacetValue:
    value: object
    count: int


@dataclass
class Facet:
    column: str
    values: list[FacetValue] = field(default_factory=list)
    truncated: bool = False
    timed_out: bool = False


def run_facets(table_obj: Table, params: list[tuple[str, str]]) -> list[Facet]:
    """Compute facets for the table's opt-in facetable columns under current filters."""
    facet_columns = table_obj.facetable_columns
    if not facet_columns:
        return []

    sa_columns, _filters, where = compile_filters(table_obj, params)
    tbl = table(table_obj.name, *sa_columns.values())

    facets: list[Facet] = []
    for col_name in facet_columns:
        col = sa_columns[col_name]
        stmt = (
            select(col.label("value"), func.count().label("n"))
            .select_from(tbl)
            .group_by(col)
            .order_by(desc("n"))
            .limit(FACET_SIZE + 1)
        )
        if where:
            stmt = stmt.where(and_(*where))

        facet = Facet(column=col_name)
        try:
            with engine.connect() as conn:
                rows = conn.execute(stmt).all()
        except SQLAlchemyError:
            # Most likely a statement-timeout cancellation on a costlier-than-expected
            # column. Drop this facet only; the page still renders.
            facet.timed_out = True
            facets.append(facet)
            continue

        facet.truncated = len(rows) > FACET_SIZE
        facet.values = [
            FacetValue(value=r.value, count=int(r.n)) for r in rows[:FACET_SIZE]
        ]
        facets.append(facet)

    return facets
