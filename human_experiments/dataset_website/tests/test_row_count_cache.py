"""Row-count cache: one query per TTL, and stale-not-500 when Postgres is sick.

The index pages render ~24 tables. Before this cache each one was its own
connection and its own chance to burn the 10s statement_timeout, which is how a
memory-starved orca turned into a 500 on `/` (2026-09-05/06). These exercise the
caching and degradation logic with `_fetch_row_counts` patched out; no database
connection is needed.
"""

import pytest
from sqlalchemy.exc import OperationalError

from dataset_website import schema


@pytest.fixture(autouse=True)
def _clear_cache():
    """Reset module state so each test starts from a cold cache."""
    schema._row_counts = {}
    schema._row_counts_expires_at = 0.0
    yield
    schema._row_counts = {}
    schema._row_counts_expires_at = 0.0


def _boom(*_args, **_kwargs):
    raise OperationalError("SELECT reltuples", {}, Exception("statement timeout"))


def test_fetches_once_and_serves_from_cache(monkeypatch):
    calls = []

    def fake_fetch():
        calls.append(1)
        return {"eeg_raw": 12, "fnirs_raw": 34}

    monkeypatch.setattr(schema, "_fetch_row_counts", fake_fetch)

    assert schema.row_counts() == {"eeg_raw": 12, "fnirs_raw": 34}
    # Twenty-four more reads, i.e. a homepage render, hit no database at all.
    for _ in range(24):
        schema.row_counts()
    assert len(calls) == 1


def test_force_refetches(monkeypatch):
    calls = []

    def fake_fetch():
        calls.append(1)
        return {"eeg_raw": len(calls)}

    monkeypatch.setattr(schema, "_fetch_row_counts", fake_fetch)

    assert schema.row_counts()["eeg_raw"] == 1
    assert schema.row_counts(force=True)["eeg_raw"] == 2


def test_expiry_triggers_refetch(monkeypatch):
    calls = []
    monkeypatch.setattr(
        schema, "_fetch_row_counts", lambda: (calls.append(1), {"eeg_raw": 1})[1]
    )

    schema.row_counts()
    assert len(calls) == 1

    # Pretend the TTL elapsed.
    schema._row_counts_expires_at = 0.0
    schema.row_counts()
    assert len(calls) == 2


def test_database_failure_serves_stale_instead_of_raising(monkeypatch):
    monkeypatch.setattr(schema, "_fetch_row_counts", lambda: {"eeg_raw": 99})
    assert schema.row_counts()["eeg_raw"] == 99

    monkeypatch.setattr(schema, "_fetch_row_counts", _boom)
    schema._row_counts_expires_at = 0.0
    assert schema.row_counts() == {"eeg_raw": 99}


def test_first_ever_failure_yields_zero_not_an_error(monkeypatch):
    monkeypatch.setattr(schema, "_fetch_row_counts", _boom)
    assert schema.row_counts() == {}
    assert schema.estimated_row_count("eeg_raw") == 0


def test_failure_backs_off_rather_than_retrying_every_call(monkeypatch):
    calls = []

    def failing():
        calls.append(1)
        _boom()

    monkeypatch.setattr(schema, "_fetch_row_counts", failing)

    for _ in range(24):
        schema.row_counts()
    # One attempt for the whole page, not one per table.
    assert len(calls) == 1


def test_estimated_row_count_reads_the_snapshot(monkeypatch):
    monkeypatch.setattr(schema, "_fetch_row_counts", lambda: {"eeg_raw": 7})
    assert schema.estimated_row_count("eeg_raw") == 7
    assert schema.estimated_row_count("no_such_table") == 0
