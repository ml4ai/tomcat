# CLAUDE.md — dataset_pipeline

Guidance for Claude Code in this subtree. See the repo-root `CLAUDE.md` for the
monorepo's overall layout. This project is self-contained (Python + `make`) and
does **not** use the top-level CMake build. The public site that serves what
this pipeline builds is the sibling project `../dataset_website/`; the two
share no code, only the Postgres cluster and (optionally) a `.env` file.

## Purpose

Turns raw experiment recordings into a queryable database, then exports the
bulk-download artifacts. The canonical store is the **`tomcat` database on
orca's system Postgres 15 cluster** (local socket `/var/run/postgresql`; the
old gauss-era cluster at `/space/paulosoares/postgres` is retired).

Pipeline: raw files (XDF/CSV/audio) → `update_raw` (ingest) → `sync_*` (align
signals to a 200 Hz master clock) → artifacts (`to_sqlite`, `pg_dump_artifact`).

## Running it (`make`)

All operations go through the Makefile. Read `README.md` for the prose version.

- Default is **development** mode → SQLite at `${artifact_dir}/.dev/tomcat.db`
  (`artifact_dir` comes from `.env`; the code default `/space/$USER/tomcat` is a
  gauss-era leftover). Safe.
- To touch **production** Postgres, prefix every command:
  `working_env=production db_pass=<pass> make <target>`
- Key targets: `create_tables`, `update_raw` (incremental — skips processed sessions;
  scope with `TBS=<tables>`), `sync_all` (= `sync_fnirs sync_eeg sync_gsr sync_ekg`),
  `to_sqlite` (streams to `tomcat-core.db`, signal tables excluded by default;
  resumable; run in tmux), `pg_dump_artifact`, `sync_comments`
  (imports `../dataset_website/metadata.yml` prose into Postgres COMMENTs),
  `lint` (ruff check+format).
- Scripts in `bin/` back these targets one-for-one (`populate_raw_tables.py` ← `update_raw`,
  `sync_raw_signals.py` ← `sync_*`, `to_sqlite.py`, `create_tables.py`, etc.).

## Architecture (package `dataset_pipeline/`)

SQLAlchemy ORM (2.0 `Mapped` style) + Pydantic-Settings config.

- `common/config.py` — settings read from env/`.env`: `working_env`, `db_pass`, `db_user`,
  `db_host`/`port`/`name`, `artifact_dir`, `data_root_dir`. **This is where the dev/prod
  switch lives**, and all raw-input paths are derived from `data_root_dir` (per-file
  overrides exist for the date-stamped survey exports). Check here first when paths or
  DB connections misbehave.
- `database/config.py` — builds the SQLAlchemy engine (SQLite vs Postgres) from those settings.
- `database/entity/` — the ORM models, grouped by kind:
  - `base/` — reference tables (participant, station, group_session, task, modality, …)
  - `signal/` — raw per-modality signal tables (eeg, fnirs, gaze, audio_vocalics, screen_capture)
  - `derived/` — `*_sync` tables (same columns as raw + a `frequency` field)
  - `task/` — per-task event/observation tables (minecraft, rest_state, affective, ping_pong, …)
- `raw/` — ingestion. `raw/common/process_raw_signals.py` is the shared insertion engine
  (`insert_raw_unlabeled_data` then `label_data`); `process_*_raw_data.py` are thin
  per-modality adapters that supply channel/station/transform functions.
- `derived/` — signal synchronization. `helper/modality.py` defines an abstract
  `ModalityHelper` (load → filter → up_sample → sync_to_clock → save); concrete helpers
  per modality are registered in `helper/factory.py`. `main_clock.py` defines the master clock.
- `model/audio/` — vocalics: shells out to the `SMILExtract` CLI (OpenSMILE), not a Python
  API; the .conf files it reads are in `asset/opensmile/` (the vendored OpenSMILE source
  tree itself is at the repo root, `external/opensmile/`).
- `legacy_datasette/` — the parked Datasette-era serving layer; see its README. Don't
  build on it.

## Adding a new signal/event modality

The pattern (use an existing modality like `eeg` as the template): define the ORM entity
under `database/entity/signal/`, import it in `bin/create_tables.py` so the schema
materializes, add a `raw/process_<x>_raw_data.py` adapter that calls the shared
`insert_raw_unlabeled_data`/`label_data` helpers, and register it in the
`populate_raw_tables.py` orchestrator. For synchronization, add a `*_sync` entity, a
`derived/helper/<x>.py` subclass of `ModalityHelper`, register it in `factory.py`, and add
a `sync_<x>` Makefile target. Document columns in `../dataset_website/metadata.yml`
(then `make sync_comments`).

## Gotchas

- Raw signals are inserted with `participant_id = -1` and relabeled afterward via the
  `data_validity` table; an unmatched row keeps `-1`.
- Signal IDs come from per-table `get_next_id()` (sequential per group_session+station), not
  a DB sequence — concurrent writers to the same table will collide.
- Two on-disk XDF layouts exist (v1: per-station files; v2: unified per-group); ingestion
  detects which.
