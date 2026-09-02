"""Settings for the public web app — deliberately tiny.

The website connects to Postgres only as the read-only `tomcat_public` role
(see bin/setup_public_role.sql), so it needs none of the pipeline's read-write
credentials or experiment-input paths. Keeping its own Settings class (rather
than importing the pipeline's) is what lets the two projects share nothing at
import time; `extra="ignore"` means a host can still point both at one .env.
"""

import os

from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    # Real environment variables take precedence over the file. TOMCAT_ENV_FILE
    # lets one checkout target different hosts (e.g. .env.orca); the systemd
    # unit relies on .env resolving relative to WorkingDirectory.
    model_config = SettingsConfigDict(
        env_file=os.getenv("TOMCAT_ENV_FILE", ".env"), extra="ignore"
    )

    db_host: str = "localhost"
    db_port: int = os.getenv("POSTGRES_PORT", 5433)
    db_name: str = os.getenv("TOMCAT_DB_NAME", "tomcat")

    # Read-only public role; password set out of band (setup_public_role.sql).
    web_db_user: str = "tomcat_public"
    web_db_pass: str = ""

    # Where the offline bulk artifacts (tomcat-core.db, tomcat.dump) land; the
    # /download page serves them from here.
    artifact_dir: str = f"/space/{os.getenv('USER')}/tomcat"


settings = Settings()
