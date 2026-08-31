"""The SQL-console guard: only single read-only SELECT/WITH statements pass."""

import pytest

from dataset_website.query import InvalidQuery, validate


@pytest.mark.parametrize(
    "sql, expected",
    [
        ("SELECT 1", "SELECT 1"),
        ("select 1;", "select 1"),  # trailing semicolon stripped
        ("  SELECT * FROM eeg_raw LIMIT 10  ", "SELECT * FROM eeg_raw LIMIT 10"),
        (
            "WITH x AS (SELECT 1) SELECT * FROM x",
            "WITH x AS (SELECT 1) SELECT * FROM x",
        ),
        ("-- a comment\nSELECT 1", "-- a comment\nSELECT 1"),
        # A semicolon inside a string literal is not a statement separator.
        ("SELECT ';drop table t' AS a", "SELECT ';drop table t' AS a"),
    ],
)
def test_valid_queries_pass(sql, expected):
    assert validate(sql) == expected


@pytest.mark.parametrize(
    "sql",
    [
        "",
        "   ",
        "INSERT INTO t VALUES (1)",
        "UPDATE t SET a = 1",
        "DELETE FROM t",
        "DROP TABLE t",
        "TRUNCATE t",
        "GRANT SELECT ON t TO bob",
        "CREATE TABLE t (a int)",
        "SET statement_timeout = 0",
        "COPY t TO '/tmp/x'",
        "SELECT 1; DROP TABLE t",  # multiple statements
        "SELECT 1; SELECT 2",  # multiple statements, both SELECT
    ],
)
def test_invalid_queries_rejected(sql):
    with pytest.raises(InvalidQuery):
        validate(sql)
