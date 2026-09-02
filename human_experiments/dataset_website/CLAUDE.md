# CLAUDE.md — dataset_website

Guidance for Claude Code in this subtree. See the repo-root `CLAUDE.md` for the
monorepo's overall layout. A self-contained FastAPI app (package
`dataset_website/`) that serves the live Postgres cluster built by the sibling
project `../dataset_pipeline/`. The two share no code — only the cluster and
(optionally) a `.env` file. Postgres-only by design: there is **no SQLite
path**.

## Security model (the whole point — read before touching it)

The app connects as a dedicated, powerless role `tomcat_public` created by
`bin/setup_public_role.sql` (`make setup_public_role`). That role can only
`SELECT`, has a 10s `statement_timeout`, and `default_transaction_read_only = on`;
the same timeout/read-only is *also* pinned at the connection level in `db.py`.
So an arbitrary public SQL console is safe — the **database** refuses writes and
long queries regardless of app code. `tomcat_public` is **independent of** the
internal `tomcat_readers` group (lab members) so its limits never apply to them.
Set its password out of band and put it in `.env` as `WEB_DB_PASS`.

**Postgres is self-describing.** Table/column structure comes from reflection;
human descriptions come from native `COMMENT ON TABLE/COLUMN` (authored in
`metadata.yml` here, imported by the pipeline's `make sync_comments`); the
public table set is exactly what `tomcat_public` may `SELECT` — governed by the
explicit allowlist in `setup_public_role.sql`, which is fail-closed: a table is
private unless named there, so internal and intermediate tables (e.g.
`fnirs_tmp`) stay off the public site without anyone having to remember. The
only thing not in Postgres is which columns are facetable: `config.py`
`FACET_COLUMNS` (opt-in; keep to low-cardinality columns).

## Module map

`db.py` (read-only engine), `settings.py` (six env fields — deliberately not
the pipeline's Settings), `schema.py` (cached reflection + comments + privilege
filter), `browse.py` (compact `col__op=value` URL grammar → SQLAlchemy Core
SELECT — a deliberate compatibility contract with old Datasette links; don't
change routes), `query.py` (SELECT/WITH-only guard via sqlparse, then run
capped), `facets.py` (opt-in GROUP BY counts), `app.py` (routes + CSV/JSON
export + `/download`). Templates in the package's `templates/` override;
project-root `templates/pages/*` (the curated content pages) are reused via a
Jinja `ChoiceLoader`.

## Running it

- Dev: `make launch_webapp` (uvicorn `--reload`, port 8002).
- Tests: `make test` (pure unit tests; no database needed).
- **Prod: a systemd unit on orca**, `deploy/tomcat-webapp.service` (runs
  uvicorn from the project `.venv`, binds 0.0.0.0:8002 — firewall it to the
  Caddy host with ufw — connects as `tomcat_public` over the local socket;
  Caddy on starfish reverse-proxies `tomcat.lab.pyarelal.xyz` → `orca:8002`).
  Deliberately **not** a Docker service.
- Indexes: `make create_public_indices`. One-time provisioning order: the
  pipeline's `provision_postgres.sh` → `make setup_public_role` (+ set the
  password & pg_hba line) → pipeline `create_tables`/`update_raw`/`sync_*` →
  pipeline `make sync_comments` → `make create_public_indices`.

## Bulk download

The live app is for browse/ad-hoc queries only (10s cap, capped export).
Whole-dataset grabs are pre-generated static files linked from `/download`: the
SQLite `tomcat-core.db` (every table except the seven ~400 GB signal tables)
and a compressed pg_dump `tomcat.dump` (everything), produced offline by the
pipeline (`make to_sqlite` / `make pg_dump_artifact`) into `ARTIFACT_DIR`.
`BULK_ARTIFACTS` in `app.py` is the filename allowlist; anything else in the
directory is invisible.
In prod let Caddy serve `/downloads/*` directly.

## Gotchas

- The repo-root `.gitignore` has `*.pdf`; the consent/recruitment PDFs under
  `static/supporting-material/` are tracked via `git add -f`. A new PDF there
  needs the same.
- `dataset_website/message_specs/` is vendored (rsynced from
  `ml4ai/minecraft_testbed`) — don't lint or hand-edit it.
