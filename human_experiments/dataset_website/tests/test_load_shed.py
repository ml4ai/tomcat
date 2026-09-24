"""Load shedding: the expensive routes get a fixed number of slots, and the
request that finds none waiting is answered 503 rather than queued. No database
connection is needed."""

import threading

import pytest
from fastapi import HTTPException

from dataset_website import load_shed
from dataset_website.db import MAX_OVERFLOW, POOL_SIZE


@pytest.fixture(autouse=True)
def _fresh_slots(monkeypatch):
    monkeypatch.setattr(
        load_shed, "_slots", threading.BoundedSemaphore(load_shed.QUERY_SLOTS)
    )
    monkeypatch.setattr(load_shed, "ACQUIRE_TIMEOUT_S", 0.01)


def test_slots_leave_connections_for_the_rest_of_the_site():
    assert 0 < load_shed.QUERY_SLOTS < POOL_SIZE + MAX_OVERFLOW


def test_sheds_when_every_slot_is_held():
    held = []
    for _ in range(load_shed.QUERY_SLOTS):
        cm = load_shed.query_slot()
        cm.__enter__()
        held.append(cm)

    with pytest.raises(HTTPException) as exc, load_shed.query_slot():
        pass
    assert exc.value.status_code == 503
    assert exc.value.headers["Retry-After"] == str(load_shed.RETRY_AFTER_S)

    for cm in held:
        cm.__exit__(None, None, None)
    with load_shed.query_slot():
        pass


def test_slot_is_released_when_the_query_fails():
    for _ in range(load_shed.QUERY_SLOTS + 1):
        with pytest.raises(RuntimeError), load_shed.query_slot():
            raise RuntimeError("statement timeout")
    with load_shed.query_slot():
        pass
