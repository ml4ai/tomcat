lint:
	black .
	isort .
	autoflake -r . --in-place --remove-all-unused-imports
	flake8 coordination

vocalics:
	./bin/extract_vocalic_features.py
