from __future__ import annotations

import logging
import os
import sys
from typing import Optional

from pydantic import field_validator, model_validator
from pydantic_settings import BaseSettings, SettingsConfigDict

DEVELOPMENT = "development"
PRODUCTION = "production"
VALID_ENVIRONMENTS = [DEVELOPMENT, PRODUCTION]


class Settings(BaseSettings):
    # Load a single env file (gitignored) so a host's whole configuration lives in one
    # place instead of being exported per-invocation. Real environment variables still
    # take precedence over the file, so `working_env=production make ...` keeps working.
    # TOMCAT_ENV_FILE lets one checkout target different hosts (e.g. .env.orca).
    model_config = SettingsConfigDict(
        env_file=os.getenv("TOMCAT_ENV_FILE", ".env"), extra="ignore"
    )

    db_pass: str = ""
    db_host: str = "localhost"
    db_user: str = os.getenv("USER")
    db_port: int = os.getenv("POSTGRES_PORT", 5433)
    db_name: str = os.getenv("TOMCAT_DB_NAME", "tomcat")
    working_env: str = DEVELOPMENT

    # Credentials for the public-facing web app's read-only connection. The app
    # connects as `tomcat_public` (see bin/setup_public_role.sql), a role that can
    # only SELECT and is subject to a server-side statement timeout. Kept separate
    # from db_user/db_pass (the read-write pipeline connection) so the two never
    # share a privilege level. web_db_pass is empty in development (SQLite, unused).
    web_db_user: str = "tomcat_public"
    web_db_pass: str = ""
    artifact_dir: str = "/media/snail-ssd/tomcat/artifacts"
    image_url_root_dir: str = "https://ivilab.cs.arizona.edu/data/tomcat/group"

    # Root of the raw experiment inputs (read-only). All input paths below are
    # derived from it unless set explicitly (the per-file overrides exist because
    # the REDCap survey exports carry a date stamp in their filename, so a
    # re-export changes the name without changing the root).
    data_root_dir: str = (
        "/media/snail-ssd/tomcat/data/raw/LangLab/experiments/study_3_pilot"
    )
    experiment_root_dir: Optional[str] = None
    data_validity_workbook_path: Optional[str] = None
    station_to_eeg_workbook_path: Optional[str] = None
    station_to_minecraft_playername_mapping_path: Optional[str] = None
    self_report_data_path: Optional[str] = None
    self_report_data_dictionary_path: Optional[str] = None
    post_game_survey_data_path: Optional[str] = None
    post_game_survey_data_dictionary_path: Optional[str] = None

    @model_validator(mode="after")
    def derive_data_paths(self):
        root = self.data_root_dir.rstrip("/")
        tails = {
            "experiment_root_dir": "group",
            "data_validity_workbook_path": "data_validity_table.csv",
            "station_to_eeg_workbook_path": "station_to_eeg_amp_mapping.csv",
            "station_to_minecraft_playername_mapping_path": (
                "station_to_minecraft_playername_mapping.csv"
            ),
            "self_report_data_path": "ToMCATSelfReport_DATA_2023-06-11_2358.tsv",
            "self_report_data_dictionary_path": (
                "ToMCATSelfReport_DataDictionary_2023-08-08.tsv"
            ),
            "post_game_survey_data_path": (
                "ToMCATPostGameSurvey_DATA_2024-07-16_2050.csv"
            ),
            "post_game_survey_data_dictionary_path": (
                "ToMCATPostGameSurvey_DataDictionary_2024-07-17.csv"
            ),
        }
        for field, tail in tails.items():
            if getattr(self, field) is None:
                setattr(self, field, f"{root}/{tail}")
        return self

    @classmethod
    @field_validator(
        "db_pass",
        "db_host",
        "db_user",
        "db_port",
        "db_name",
        "working_env",
    )
    def check_not_empty(cls, v):
        assert v != "", f"{v} is not defined."
        return v

    @classmethod
    @field_validator("working_env")
    def check_env_in_list(cls, v):
        if v not in VALID_ENVIRONMENTS:
            raise ValueError(
                "WORKING_ENV set to an invalid value. Must be one of "
                f"{', '.join(VALID_ENVIRONMENTS)}"
            )
        return v


settings = Settings()

if settings.working_env == DEVELOPMENT:
    RUN_DIR = f"{settings.artifact_dir}/.dev"
else:
    RUN_DIR = settings.artifact_dir

LOG_DIR = f"{RUN_DIR}/log"
TMP_DIR = f"{RUN_DIR}/tmp"

os.makedirs(LOG_DIR, exist_ok=True)
os.makedirs(TMP_DIR, exist_ok=True)


def configure_logging(log_file_path: str) -> None:
    """Configure root logging to write to ``log_file_path`` and stderr.

    Each processor calls this from inside its entry function so that the logs
    end up in the processor's own file. ``logging.basicConfig`` is a no-op once
    handlers are configured, so we reset the root handlers here to make repeated
    configuration take effect.
    """
    root_logger = logging.getLogger()
    root_logger.setLevel(logging.INFO)
    for handler in list(root_logger.handlers):
        root_logger.removeHandler(handler)
    root_logger.addHandler(logging.FileHandler(filename=log_file_path, mode="a"))
    root_logger.addHandler(logging.StreamHandler(stream=sys.stderr))
