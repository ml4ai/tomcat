#!/usr/bin/env bash
# =============================================================================
# provision_postgres.sh
#
# One-time (idempotent) bootstrap of the ToMCAT Postgres database and its
# access-control roles on a host's local cluster. Safe to re-run.
#
# Creates:
#   - group roles  tomcat_readers (read)  and  tomcat_curators (write), NOLOGIN
#   - database     tomcat  owned by tomcat_curators
#   - schema grants + ALTER DEFAULT PRIVILEGES so future tables auto-grant SELECT
#     to readers
#
# Per-user LOGIN roles and their group membership are managed separately (peer
# auth + the Kanidm->PG reconcile script); this script only bootstraps the first
# curator passed as CURATOR (default: the invoking user) so the initial build can
# create the schema.
#
# Usage:
#   ./bin/provision_postgres.sh                 # curator = $USER, db = tomcat
#   CURATOR=adarsh DB=tomcat ./bin/provision_postgres.sh
#
# Requires: a Postgres SUPERUSER connection over local peer auth (i.e. run as a
# superuser OS account, or set PGUSER/PGHOST accordingly).
# =============================================================================
set -euo pipefail

DB="${DB:-tomcat}"
CURATOR="${CURATOR:-$(id -un)}"
# Maintenance connection (peer auth on the local socket by default).
PSQL=(psql -v ON_ERROR_STOP=1 -X -q)

echo ">> Provisioning database '${DB}' with curator '${CURATOR}'"

# --- 1. Group roles (idempotent) --------------------------------------------
"${PSQL[@]}" -d postgres <<'SQL'
DO $$
BEGIN
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'tomcat_readers') THEN
    CREATE ROLE tomcat_readers NOLOGIN;
  END IF;
  IF NOT EXISTS (SELECT FROM pg_roles WHERE rolname = 'tomcat_curators') THEN
    CREATE ROLE tomcat_curators NOLOGIN;
  END IF;
END
$$;
SQL

# --- 2. Database owned by the curators group (idempotent via \gexec) --------
"${PSQL[@]}" -d postgres <<SQL
SELECT format('CREATE DATABASE %I OWNER tomcat_curators', '${DB}')
WHERE NOT EXISTS (SELECT FROM pg_database WHERE datname = '${DB}')\gexec
SQL

# --- 3. Schema privileges + default privileges (run inside the target DB) ---
"${PSQL[@]}" -d "${DB}" <<SQL
-- Only the two groups (and superusers) may connect.
REVOKE CONNECT ON DATABASE ${DB} FROM PUBLIC;
GRANT  CONNECT ON DATABASE ${DB} TO tomcat_readers, tomcat_curators;

-- Schema usage/create.
GRANT USAGE          ON SCHEMA public TO tomcat_readers;
GRANT USAGE, CREATE  ON SCHEMA public TO tomcat_curators;

-- Existing objects (no-op on first run; covers re-runs after tables exist).
GRANT SELECT             ON ALL TABLES    IN SCHEMA public TO tomcat_readers;
GRANT SELECT, USAGE      ON ALL SEQUENCES IN SCHEMA public TO tomcat_readers;
GRANT ALL                ON ALL TABLES    IN SCHEMA public TO tomcat_curators;
GRANT ALL                ON ALL SEQUENCES IN SCHEMA public TO tomcat_curators;

-- Future objects created BY the curators group auto-grant read to readers.
ALTER DEFAULT PRIVILEGES FOR ROLE tomcat_curators IN SCHEMA public
  GRANT SELECT ON TABLES TO tomcat_readers;
ALTER DEFAULT PRIVILEGES FOR ROLE tomcat_curators IN SCHEMA public
  GRANT SELECT, USAGE ON SEQUENCES TO tomcat_readers;
SQL

# --- 4. Bootstrap the first curator so the initial build owns objects as the group
"${PSQL[@]}" -d "${DB}" <<SQL
GRANT tomcat_curators TO "${CURATOR}";
-- Sessions this curator opens against ${DB} assume the group role, so tables they
-- create are owned by tomcat_curators (consistent ownership + default privileges).
ALTER ROLE "${CURATOR}" IN DATABASE ${DB} SET role TO tomcat_curators;
SQL

echo ">> Done. Roles: tomcat_readers (read), tomcat_curators (write). DB: ${DB}."
echo ">> Add readers/curators by granting membership (see the Kanidm->PG reconcile"
echo "   script handoff); new curators also need:"
echo "     ALTER ROLE \"<user>\" IN DATABASE ${DB} SET role TO tomcat_curators;"
