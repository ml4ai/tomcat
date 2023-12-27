lint:
	black .
	isort .
	autoflake -r . --in-place --remove-all-unused-imports
	flake8 .

screenshots_to_server:
	./bin/copy_screenshots_to_server

create_datasette:
	./bin/create_datasette_with_plugins_image

generate_diagram:
	./bin/generate_database_diagram

launch_datasette:
	./bin/launch_dev

update_inspect_file:
	./bin/update_inspect_file

clear_db:
	@PYTHONPATH="." ./bin/clear_database.py

create_tables:
	@PYTHONPATH="." ./bin/create_tables.py
	@PYTHONPATH="." ./bin/create_table_indices.py

update_raw:
	@PYTHONPATH="." ./bin/populate_raw_tables.py

extract_vocalics:
	@PYTHONPATH="." ./bin/extract_vocalic_features.py

sync_raw:
	# We up-sample such that the number of samples is 10x the frequency of the clock. For fNIRS
	# (10Hz hardware frequency), we up-sample 200 times such that we have samples at 2000Hz to
	# interpolate with the main clock with frequency 200Hz. EEG is originally sampled at 500Hz, so
	# we only need to up-sample it by 4 times to reach 2000Hz.
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="fnirs" --clock_frequency=200 \
		--up_sample_scale=200
	@PYTHONPATH="." ./bin/sync_raw_signals.py --modality="eeg" --clock_frequency=200 \
		--up_sample_scale=4
