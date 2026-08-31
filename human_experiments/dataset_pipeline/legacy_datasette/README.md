# legacy_datasette — parked, do not build on

The Datasette/SQLite serving layer this repo used before the FastAPI site
(`../../dataset_website/`) replaced it. Parked here — not deleted — because
`tomcat.ivilab.org` still runs a Datasette instance from its own checkout, and
the decision about that host's fate (retire / redirect / errata-note) is still
open. When that decision lands, this directory can be deleted; everything stays
recoverable from git history.

Contents:

- `docker-compose.yml` / `docker-compose.prod.yml` — the Datasette containers.
- `plugins/metadata.py` — Datasette plugin injecting `metadata.yml` prose
  (that file now lives at `../../dataset_website/metadata.yml`; a revived
  Datasette would need to point there or take a copy).
- `bin/launch_dev`, `bin/create_datasette_with_plugins_image`,
  `bin/update_inspect_file` — dev/serve scripts (hardcode
  `/space/$USER/tomcat/.dev/tomcat.db`).
- `bin/generate_database_diagram` — renders `static/db_diagram.png` from the
  SQLite artifact via `visualize-sqlite`.
- `inspect-data.json` — Datasette's inspect cache for the SQLite build.
- `templates/base.html`, `index.html`, `table.html` — Datasette template
  overrides (the FastAPI site has its own).

The old Makefile targets (`create_datasette`, `launch_datasette`,
`update_inspect_file`, `generate_diagram`) were removed with the split; run the
scripts directly if ever needed. Python deps lived in the old `serve` extra:
`datasette~=0.64.3`, `datasette-render-markdown`, `datasette-pretty-json`
(`db-to-sqlite[postgresql]` survives as the pipeline's `sqlite_export` extra).
