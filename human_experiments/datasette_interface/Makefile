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

update_db:
	@PYTHONPATH="." ./bin/populate_raw_tables.py

update_screen_capture:
	@PYTHONPATH="." ./bin/populate_raw_tables.py --include="screen_capture"

lint:
	black .
	isort .
	autoflake -r . --in-place --remove-all-unused-imports
	flake8 .