lint:
	ruff check --fix .
	ruff format .

# Miscellaneous
screenshots_to_server:
	./bin/copy_screenshots_to_server.sh

# Database operations
create_tables:
	@PYTHONPATH="." ./bin/create_tables.py
	@PYTHONPATH="." ./bin/create_table_indices.py

# Import metadata.yml descriptions (from the sibling dataset_website project)
# into native Postgres COMMENTs — the source the public web app reads. Run with
# the read-write pipeline connection.
sync_comments:
	@PYTHONPATH="." ./bin/sync_table_comments.py

# Bulk-download artifact: a compressed, pg_restore-able custom-format dump plus a
# sha256 sidecar, written to ARTIFACT_DIR. Run offline (it can be slow); the web
# app's /download page links it. The SQLite artifact is produced by `to_sqlite`.
pg_dump_artifact:
	./bin/dump_database

update_raw:
	@PYTHONPATH="." ./bin/populate_raw_tables.py

# We up-sample such that the number of samples is 10x the frequency of the clock. For fNIRS
# (10Hz hardware frequency), we up-sample 200 times such that we have samples at 2000Hz to
# interpolate with the main clock with frequency 200Hz. EEG is originally sampled at 500Hz, so
# we only need to up-sample it by 4 times to reach 2000Hz.
sync_fnirs:
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="fnirs" --clock_frequency=200 \
		--up_sample_scale=200

sync_eeg:
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="eeg" --clock_frequency=200 \
		--up_sample_scale=4

sync_gsr:
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="gsr" --clock_frequency=200 \
		--up_sample_scale=4

sync_ekg:
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="ekg" --clock_frequency=200 \
		--up_sample_scale=4

sync_all: sync_fnirs sync_eeg sync_gsr sync_ekg

to_sqlite:
	@PYTHONPATH="." working_env=production ./bin/to_sqlite.py
