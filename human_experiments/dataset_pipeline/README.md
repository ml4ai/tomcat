This project is responsible for parsing files saved during the experiments and saving them to a database. The main database lives in a Postgres cluster. The public web interface that serves it is the sibling project `../dataset_website/`; this project also generates the bulk-download artifacts (SQLite copy, pg_dump) that the site links.

# Prerequisites
Dependencies are declared in `pyproject.toml` and split into optional groups so you
only install what a given task needs. Using [uv](https://docs.astral.sh/uv/):
```
uv venv && source .venv/bin/activate
uv pip install -e .                     # core: base + task + raw-signal ingestion
uv pip install -e '.[signal]'           # + derived sync_* step (scipy/neurokit2/mne)
uv pip install -e '.[audio]'            # + vocalics (heavy: torch/whisper)
uv pip install -e '.[sqlite_export]'    # + db-to-sqlite (the to_sqlite target)
uv pip install -e '.[all]'              # everything
```
Configuration is read from a `.env` file (see `.env.example`); copy it to `.env` and edit.

# Populating the ToMCAT database

The tomcat dataset is in the `tomcat` database on orca's system Postgres 15 cluster, reached over the local socket (`/var/run/postgresql`). A series of `make` commands can be used to perform different operations on this dataset. The commands run by default in `development` mode, which uses an sqlite database created under `${ARTIFACT_DIR}/.dev` (from `.env`). To modify the Postgres database, the commands need to be executed in `production` mode, which can be done by adding the following before each command:
```
working_env=production db_pass=<user_postgres_pass>
```
Your user must have permission to modify the Postgres database in production mode.

## Commands
1. **create_tables**: creates new tables and indices. If a table already exists, it won't be changed.
2. **update_raw**: Adds new raw data to the relevant tables. This can be called to update the database with new experiment data. It will skip experiments already processed. The environment variable `TBS` can be used in conjunction with this command to specify a subset of tables one wants to update.
3. **sync_all** (or per-modality `sync_fnirs`/`sync_eeg`/`sync_gsr`/`sync_ekg`): Filters and synchronizes signals with a main clock with frequency 200Hz that starts 1 minute before the rest_state task and ends 1 minute after end of the last minecraft trial. Synchronized signals are saved to the `*_sync` tables.
4. **sync_comments**: Imports table/column descriptions from `../dataset_website/metadata.yml` into native Postgres COMMENTs (what the public site displays).
5. **to_sqlite**: Copies the Postgres database to an SQLite bulk-download artifact in `ARTIFACT_DIR`. It runs in production mode automatically to make sure to read from the Postgres database. The environment variable `TBS` can be used in conjunction with this command to specify a subset of tables one wants to copy, otherwise, all tables will be copied. Be patient and run this in a tmux session as this process can take several days depending on the size of the tables.
6. **pg_dump_artifact**: Writes a compressed, pg_restore-able dump (plus sha256 sidecar) to `ARTIFACT_DIR`.

# Miscellaneous
1. **screenshots_to_server**: Copies screenshot images to the web server for access though a public URL.

The Datasette-era serving layer (Docker images, inspect file, database diagram)
is parked in `legacy_datasette/` — see its README.
