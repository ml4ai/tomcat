sync_fnirs:
	python data_pre_processing/scripts/sync/sync_raw_signals.py --modality="fnirs" --recording_freq=10 --upsampling_freq=500 --final_freq=120 --n_jobs=10 --db_passwd="${TOMCAT_DB_PASSWD}"

lint:
	black .
	isort .
	autoflake -r . --in-place --remove-all-unused-imports
	flake8 .

vocalics:
	./bin/extract_vocalic_features.py
