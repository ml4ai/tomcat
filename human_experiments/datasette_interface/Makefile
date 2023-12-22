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

update_db_raw:
	./recreate_database --db_user="${USER}" --db_port=5433 --db_name="tomcat" \
		--db_passwd="${TOMCAT_DB_PASSWD}"

screen_capture:
	./recreate_database --db_user="${USER}" --db_port=5433 --db_name="tomcat" \
		--db_passwd="${TOMCAT_DB_PASSWD}" --no_base --no_rest_state --no_affective \
		--no_finger_tapping --no_ping_pong_comp --no_ping_pong_coop --no_minecraft \
		--no_fnirs --no_eeg --no_gaze --override

lint:
	black .
	isort .
	autoflake -r . --in-place --remove-all-unused-imports
	flake8 .