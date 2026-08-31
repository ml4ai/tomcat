lint:
	ruff check --fix .
	ruff format .

test:
	pytest tests/

# Serve the site in the foreground with --reload for DEV; set WEB_DB_PASS in
# .env first. In production it runs as a systemd unit
# (deploy/tomcat-webapp.service), not via this target.
launch_webapp:
	./bin/launch_webapp

# Provision the read-only role the web app connects as. Run as a Postgres
# superuser / tomcat_curators over local peer auth, AFTER the pipeline's
# provision_postgres.sh. Set the password out of band afterwards (see the SQL
# file header).
setup_public_role:
	# SET ROLE NONE drops to the superuser session_user first: on orca, curators are
	# auto-assumed per-database (ALTER ROLE ... IN DATABASE ... SET role), which would
	# otherwise lack CREATEROLE. A no-op when connecting as a plain superuser.
	psql -v ON_ERROR_STOP=1 -d $(or $(TOMCAT_DB_NAME),tomcat) \
		-c "SET ROLE NONE" -f bin/setup_public_role.sql

# Indexes that speed up the web app's browse/facet queries. Run as a curator.
create_public_indices:
	psql -v ON_ERROR_STOP=1 -d $(or $(TOMCAT_DB_NAME),tomcat) -f bin/create_public_indices.sql
