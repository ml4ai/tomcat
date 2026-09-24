"""Load shedding for the database-heavy routes (table browse, SQL console).

The engine holds at most POOL_SIZE + MAX_OVERFLOW connections, and a filtered
browse can hold one for up to the 10s statement timeout. On 2026-09-24 a
residential-proxy crawler (one request per IP, spoofed browser UAs) sent ~450
filtered browse requests a minute; every one queued for a connection, the pool
ran out, and the homepage and the uptime probe timed out with everything else.
Caddy can block that crawler by its headers, but only until it changes them.

So the expensive routes share a fixed number of slots, fewer than the pool
holds, and a request that cannot get one within a second is answered 503 with
Retry-After instead of queueing. A flood then costs its own requests, and the
reserved connections keep the rest of the site answering.
"""

from __future__ import annotations

import threading
from collections.abc import Iterator
from contextlib import contextmanager

from fastapi import HTTPException

from dataset_website.db import MAX_OVERFLOW, POOL_SIZE

# Connections left for everything that is not browse or the SQL console: the
# homepage's row counts, the schema pages, the probe.
RESERVED_CONNECTIONS = 4
QUERY_SLOTS = POOL_SIZE + MAX_OVERFLOW - RESERVED_CONNECTIONS

# How long a request waits for a slot before it is shed. Short on purpose: a
# queue of waiting requests is exactly what took the site down.
ACQUIRE_TIMEOUT_S = 1.0
RETRY_AFTER_S = 30

_slots = threading.BoundedSemaphore(QUERY_SLOTS)


@contextmanager
def query_slot() -> Iterator[None]:
    """Hold one of the QUERY_SLOTS for the duration of the block, or raise 503."""
    if not _slots.acquire(timeout=ACQUIRE_TIMEOUT_S):
        raise HTTPException(
            status_code=503,
            detail="The dataset browser is busy. Please retry shortly.",
            headers={"Retry-After": str(RETRY_AFTER_S)},
        )
    try:
        yield
    finally:
        _slots.release()
