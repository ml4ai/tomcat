"""Browse engine: URL query params -> validated filters / WHERE clauses.

These exercise the pure translation logic (no database connection needed).
"""

from dataset_website.browse import compile_filters, parse_filters
from dataset_website.schema import Column, Table


def _table() -> Table:
    return Table(
        name="eeg_raw",
        description="",
        columns=[
            Column("id", "INTEGER", nullable=False, primary_key=True, is_numeric=True),
            Column(
                "station", "TEXT", nullable=True, primary_key=False, is_numeric=False
            ),
            Column("task", "TEXT", nullable=True, primary_key=False, is_numeric=False),
        ],
        primary_keys=["id"],
    )


def test_bare_param_is_exact_filter():
    f = parse_filters(_table(), [("station", "lion")])
    assert len(f) == 1
    assert (f[0].column, f[0].op, f[0].value) == ("station", "exact", "lion")
    assert f[0].label == "station = lion"


def test_operator_suffix_parsed():
    f = parse_filters(_table(), [("id__gt", "5")])
    assert (f[0].column, f[0].op, f[0].value) == ("id", "gt", "5")


def test_valueless_operator():
    f = parse_filters(_table(), [("task__isnull", "1")])
    assert f[0].op == "isnull"
    assert f[0].value is None
    assert f[0].label == "task is null"


def test_reserved_and_unknown_params_ignored():
    f = parse_filters(
        _table(),
        [
            ("_sort", "id"),  # reserved
            ("_offset", "10"),  # reserved
            ("bogus", "x"),  # unknown column
            ("station__nope", "y"),  # unknown operator
            ("station", "tiger"),  # the only valid one
        ],
    )
    assert len(f) == 1
    assert f[0].column == "station"


def test_compile_filters_builds_one_clause_per_filter():
    _cols, filters, where = compile_filters(
        _table(), [("station", "lion"), ("id__gte", "3")]
    )
    assert len(filters) == 2
    assert len(where) == 2  # SQLAlchemy expressions, not executed here
