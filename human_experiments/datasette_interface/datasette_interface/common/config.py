import os

from pydantic import field_validator
from pydantic_settings import BaseSettings

USER = os.getenv("USER")
DEVELOPMENT = "development"
PRODUCTION = "production"
VALID_ENVIRONMENTS = [DEVELOPMENT, PRODUCTION]


class Settings(BaseSettings):
    db_pass: str = ""
    db_host: str = "localhost"
    db_user: str = os.getenv("USER")
    db_port: int = 5433
    db_name: str = "tomcat"
    working_env: str = DEVELOPMENT
    artifact_dir: str = f"/space/{USER}/tomcat"
    image_url_root_dir: str = "https://ivilab.cs.arizona.edu/data/tomcat/group"
    experiment_root_dir: str = (
        "/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"
    )
    data_validity_workbook_path: str = (
        "/tomcat/data/raw/LangLab/experiments/study_3_pilot/data_validity_table.csv"
    )
    station_to_eeg_workbook_path: str = (
        "/tomcat/data/raw/LangLab/experiments/study_3_pilot/"
        "station_to_eeg_amp_mapping.csv"
    )

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

    # SQLITE_DB_PATH = f"/space/{USER}/tomcat/tomcat.db"
    # os.makedirs(f"/space/{USER}/tomcat", exist_ok=True)


settings = Settings()

if settings.working_env == DEVELOPMENT:
    RUN_DIR = f"{settings.artifact_dir}/.dev"
else:
    RUN_DIR = settings.artifact_dir

LOG_DIR = f"{RUN_DIR}/log"
TMP_DIR = f"{RUN_DIR}/tmp"

os.makedirs(LOG_DIR, exist_ok=True)
os.makedirs(TMP_DIR, exist_ok=True)
