"""Read-only Postgres engine for the public web app.

This is intentionally a *separate* engine from the pipeline's
(dataset_pipeline.database.config, the read-write connection). Here we connect
as the powerless
`tomcat_public` role, through a bounded connection pool, with a server-side
statement timeout pinned at the connection level as a backstop. The successor's
whole security model lives here: the app can only ever issue read-only,
time-bounded statements, enforced by Postgres rather than by app code alone.

The app is Postgres-only by design (no SQLite path). "Development" just means
pointing db_host/db_name at a non-production cluster via the same env vars.
"""

from __future__ import annotations

from sqlalchemy import create_engine
from sqlalchemy.engine import Engine

from dataset_website.settings import settings

# Hard ceiling enforced per-connection, independent of the role-level ALTER ROLE
# setting in bin/setup_public_role.sql. Two layers must fail for a query to run away.
STATEMENT_TIMEOUT_MS = 10_000

# Keep concurrency bounded so a burst of expensive public queries cannot exhaust
# the cluster's connection slots. Max connections held by this app == pool_size +
# max_overflow.
POOL_SIZE = 5
MAX_OVERFLOW = 5


def _build_url() -> str:
    """Build the SQLAlchemy URL for the read-only public connection."""
    user = settings.web_db_user
    pwd = settings.web_db_pass
    host = settings.db_host
    port = settings.db_port
    name = settings.db_name

    if host.startswith("/"):
        # Unix-socket directory. tomcat_public uses password (scram) auth, not peer,
        # so the password is always supplied even over the socket.
        return f"postgresql://{user}:{pwd}@/{name}?host={host}&port={port}"
    return f"postgresql://{user}:{pwd}@{host}:{port}/{name}"


def _build_engine() -> Engine:
    return create_engine(
        _build_url(),
        future=True,
        pool_size=POOL_SIZE,
        max_overflow=MAX_OVERFLOW,
        pool_pre_ping=True,
        pool_timeout=10,
        connect_args={
            # Pinned at connect time so it holds even if the role-level default is
            # ever missing. read_only here is belt-and-suspenders over the GRANTs.
            "options": (
                f"-c statement_timeout={STATEMENT_TIMEOUT_MS} "
                "-c default_transaction_read_only=on"
            ),
            # Identify the app in pg_stat_activity for easy diagnosis.
            "application_name": "tomcat_public_webapp",
        },
    )


engine: Engine = _build_engine()
