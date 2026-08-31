"""Test fixtures: set a safe environment BEFORE any dataset_website import.

settings.py instantiates Settings at import time, so the env must be
neutralised here, before collection imports the webapp modules.
"""

import os
import tempfile

os.environ.setdefault("ARTIFACT_DIR", tempfile.mkdtemp(prefix="tomcat_test_"))
os.environ.setdefault("DB_HOST", "localhost")
os.environ.setdefault("WEB_DB_PASS", "test")
# Don't let a developer's real .env bleed into the tests.
os.environ.setdefault("TOMCAT_ENV_FILE", "/nonexistent.env")
