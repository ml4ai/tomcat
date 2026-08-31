"""Public-facing, Postgres-backed web interface for the ToMCAT dataset.

A focused successor to the Datasette/SQLite interface: it serves the canonical
Postgres cluster directly (no multi-day SQLite export) through a powerless
read-only role, reimplementing only the browse + SQL-console features the public
actually uses. See the project CLAUDE.md one level up.
"""
