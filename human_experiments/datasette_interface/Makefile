lint:
	ruff check --fix .
	ruff format .

# Datasette
create_datasette:
	./bin/create_datasette_with_plugins_image

launch_datasette:
	./bin/launch_dev

# Miscellaneous
update_inspect_file:
	./bin/update_inspect_file

generate_diagram:
	./bin/generate_database_diagram

screenshots_to_server:
	./bin/copy_screenshots_to_server

# Database operations
clear_db:
	@PYTHONPATH="." ./bin/clear_database.py

create_tables:
	@PYTHONPATH="." ./bin/create_tables.py
	@PYTHONPATH="." ./bin/create_table_indices.py

update_raw:
	@PYTHONPATH="." ./bin/populate_raw_tables.py

# We up-sample such that the number of samples is 10x the frequency of the clock. For fNIRS
# (10Hz hardware frequency), we up-sample 200 times such that we have samples at 2000Hz to
# interpolate with the main clock with frequency 200Hz. EEG is originally sampled at 500Hz, so
# we only need to up-sample it by 4 times to reach 2000Hz.
sync_raw:
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="fnirs" --clock_frequency=200 \
		--up_sample_scale=200
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="eeg" --clock_frequency=200 \
		--up_sample_scale=4
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="gsr" --clock_frequency=200 \
		--up_sample_scale=4
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="ekg" --clock_frequency=200 \
		--up_sample_scale=4

to_sqlite:
	@PYTHONPATH="." working_env=production ./bin/to_sqlite.py
