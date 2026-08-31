# ToMCAT public web app — deploy runbook

The Postgres-backed successor to the Datasette interface. It serves the **live**
`tomcat` cluster directly (no SQLite export in the request path) through a powerless
read-only role, reimplementing the slice of Datasette the public uses: faceted table
browse + a read-only SQL console, plus bulk-download links.

- **Runs on orca** (next to the Postgres cluster), proxied by Caddy on the public host.
- **Postgres-only.** No SQLite path.
- **Security boundary is in Postgres:** the app connects as `tomcat_public`, a role that
  can only `SELECT`, under a 10s `statement_timeout` and a read-only transaction.

See `CLAUDE.md` in this directory for the module-level architecture. The data
itself is built by the sibling project `../dataset_pipeline/`.

---

## Prerequisites

- The cluster + base roles already exist (the pipeline's `bin/provision_postgres.sh`
  has been run, creating the `tomcat` DB and the `tomcat_curators` / `tomcat_readers`
  groups).
- The project virtualenv has the dependencies: `uv pip install -e .`
  (fastapi, uvicorn, jinja2, markdown, sqlparse, …). On orca the venv is `.venv/`.
- A `.env` in the project root (gitignored, see `.env.example`) with at least:
  `DB_HOST=/var/run/postgresql`, `TOMCAT_DB_NAME=tomcat`, `ARTIFACT_DIR=...`.
  The `WEB_DB_*` values are added in step 2 below.

## One-time provisioning (in order)

All `make` targets run from the project root. Postgres-modifying steps need a
**superuser** connection; on orca, `adarsh` is a superuser but is auto-demoted to
`tomcat_curators` inside the `tomcat` DB, so `setup_public_role` issues `SET ROLE NONE`
first (a no-op when you connect as a plain superuser like `postgres`).

1. **Create the read-only role.**
   ```bash
   make setup_public_role
   ```
   Creates `tomcat_public` (LOGIN, read-only, 10s statement timeout, etc.), grants it
   `SELECT` on an explicit 24-table allowlist. See `bin/setup_public_role.sql`.
   Exposure is **fail-closed** — a table is private unless it is named in that
   allowlist, so publishing one is a deliberate, reviewable edit to the file, and
   an internal or intermediate table the pipeline creates (e.g. `fnirs_tmp`) never
   reaches the public site. The list is authoritative rather than additive: it
   revokes first, then grants, so removing a name unpublishes the table on the
   next run. Run it with `psql -1` so that revoke/re-grant is atomic.

2. **Set the role password, reload `pg_hba`, write `.env`.**
   The role uses password (scram) auth, so `pg_hba.conf` needs a line for it *before*
   any generic `peer`/`trust` rule (first-match-wins). Add (as root):
   ```
   local   tomcat   tomcat_public                   scram-sha-256
   host    tomcat   tomcat_public   127.0.0.1/32    scram-sha-256
   host    tomcat   tomcat_public   ::1/128         scram-sha-256
   ```
   Then generate + set the password and store it (bash):
   ```bash
   PW="$(openssl rand -hex 24)"
   psql -d tomcat -v ON_ERROR_STOP=1 -c "SET ROLE NONE" -c "SELECT pg_reload_conf()" \
        -c "ALTER ROLE tomcat_public PASSWORD '$PW'"
   printf 'WEB_DB_USER=tomcat_public\nWEB_DB_PASS=%s\n' "$PW" >> .env   # or edit in place
   ```
   Verify (should print `login ok…`, then a read-only error on the write):
   ```bash
   PGPASSWORD="$PW" psql -U tomcat_public -h /var/run/postgresql -d tomcat \
        -tAc "SELECT 'login ok as '||current_user"
   PGPASSWORD="$PW" psql -U tomcat_public -h /var/run/postgresql -d tomcat \
        -tAc "CREATE TABLE _x(a int)"   # expect: cannot execute ... in a read-only transaction
   ```

3. **Load/refresh data** (in `../dataset_pipeline/` — required for non-empty tables):
   `make create_tables`, `make update_raw`, `make sync_all`. Then **`ANALYZE`** so the
   table-index row estimates are populated (the app reads `pg_class.reltuples`):
   ```bash
   psql -d tomcat -c "ANALYZE"
   ```

4. **Descriptions → Postgres comments** (the app reads docs from native COMMENTs):
   ```bash
   # in ../dataset_pipeline/ (needs the read-write connection)
   working_env=production make sync_comments
   ```
   Imports this project's `metadata.yml` prose. Watch the output for skips —
   they mean `metadata.yml` has drifted from the live schema.

5. **Browse/facet indexes:**
   ```bash
   make create_public_indices
   ```

## Running it

- **Dev** (auto-reload, foreground): `make launch_webapp` → http://127.0.0.1:8002
- **Production** (systemd on orca):
  ```bash
  sudo cp deploy/tomcat-webapp.service /etc/systemd/system/
  sudo systemctl daemon-reload
  sudo systemctl enable --now tomcat-webapp.service
  systemctl status tomcat-webapp.service
  journalctl -u tomcat-webapp.service -f
  ```
  The app binds `0.0.0.0:8002` so Caddy on the public host (starfish) can reach it over
  the network. Lock that down with ufw so **only** starfish can connect:
  ```bash
  sudo ufw allow from $(getent hosts starfish | awk '{print $1}') to any port 8002 proto tcp comment 'caddy(starfish)->tomcat-webapp'
  ```
  (Ensure `ufw status verbose` shows `Default: deny (incoming)`.) Postgres is never
  exposed — the app reaches it over the local socket. Not a Docker service
  (see `deploy/tomcat-webapp.service`).

  On the **Caddy host (starfish)**, add a reverse-proxy block (heredoc via `bash` so it
  works regardless of the login shell — fish/`printf` mangles `\n` escapes):
  ```bash
  sudo bash -c '
  cat >> /etc/caddy/Caddyfile <<EOF

  tomcat.lab.pyarelal.xyz {
      tls { dns route53 }
      reverse_proxy orca:8002
  }
  EOF
  caddy validate --config /etc/caddy/Caddyfile --adapter caddyfile && systemctl reload caddy
  '
  ```
  **TLS:** `*.lab.pyarelal.xyz` is public DNS (Route 53); the lab issues real certs via the
  DNS-01 challenge, so the block uses `tls { dns route53 }` (requires the `caddy-dns/route53`
  module in the Caddy build + AWS creds in the Caddy env). Do **not** use `tls internal`.
  If `orca` doesn't resolve on starfish, use its IP. Note: pasting the block from a mobile
  client can wrap the site address in `<…>` — make sure the first line is exactly
  `tomcat.lab.pyarelal.xyz {`, with no angle brackets, or Caddy reports "subject does not
  qualify for certificate".

## Bulk downloads

The live app is for browse/ad-hoc queries (10s cap, capped export). Whole-dataset grabs
are pre-generated static files linked from `/download`, regenerated **offline**
in `../dataset_pipeline/`:
```bash
make to_sqlite          # tomcat.db   (SQLite)            -- run in tmux; slow
make pg_dump_artifact   # tomcat.dump (pg_restore custom) + .sha256 sidecar
```
Both land in `ARTIFACT_DIR`. In production, let Caddy serve `/downloads/*` directly off
disk (range requests, no Python in the path); the app's `/downloads/<name>` route is the
fallback.

## Public cutover

During the transition the legacy Datasette (port 8001) and this app (port 8002) run side
by side. When ready, flip the Caddy route from Datasette to `127.0.0.1:8002`. The SQLite
artifact remains as a bulk download.

## Smoke check

```bash
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:8002/tomcat
# confirm it's connecting as the powerless role:
curl -s 'http://127.0.0.1:8002/tomcat/-/query?sql=SELECT%20current_user'   # -> tomcat_public
```

## Troubleshooting

- **Table index shows "0 rows" for big tables** → those tables were never `ANALYZE`d
  (reltuples = -1, treated as 0). Run `psql -d tomcat -c "ANALYZE"`.
- **`make setup_public_role` fails with "permission denied to create role"** → you're
  connected as a non-superuser (on orca the auto-assumed `tomcat_curators`). The target's
  `SET ROLE NONE` only helps if your `session_user` is a superuser; otherwise run as `postgres`.
- **App can't authenticate as `tomcat_public`** → missing/after-the-fact `pg_hba` scram
  line, or it wasn't reloaded (`SELECT pg_reload_conf()`), or `WEB_DB_PASS` in `.env`
  doesn't match the role's password.
- **A facet is missing on a table** → the column isn't in `dataset_website/config.py` `FACET_COLUMNS`,
  or it timed out (rendered as "facet timed out"). Faceting is opt-in by design.
