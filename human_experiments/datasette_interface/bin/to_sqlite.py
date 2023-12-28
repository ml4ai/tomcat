#!/usr/bin/env python

import argparse
import os

from datasette_interface.common.config import settings
from datasette_interface.database.config import SQLALCHEMY_DATABASE_URI

TABLES = {
    "data_validity",
    "eeg_device",
    "group_session",
    "modality",
    "participant",
    "station",
    "task",
    "affective_task_event",
    "finger_tapping_task_observation",
    "minecraft_mission",
    "minecraft_testbed_message",
    "ping_pong_competitive_task_observation",
    "ping_pong_cooperative_task_observation",
    "rest_state_task",
    "audio_vocalics",
    "eeg_raw",
    "fnirs_raw",
    "gaze_raw",
    "screen_capture",
    "eeg_sync",
    "ekg_sync",
    "fnirs_sync",
    "gsr_sync",
}

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Exports tables from a database to an sqlite database."
    )
    parser.add_argument(
        "--include",
        type=str,
        default=os.getenv("TBS", "all"),
        help=f"Comma-separated list of modalities to process among {TABLES}",
    )
    parser.add_argument(
        "--exclude",
        type=str,
        help="Comma-separated list of modalities to exclude among the ones in "
        "--include",
    )

    args = parser.parse_args()

    if args.include.strip() != "all":
        for table in args.include.split(","):
            table = table.strip()
            if table not in TABLES:
                raise ValueError(f"Modality ({table}) is invalid.")

    if args.exclude:
        for table in args.exclude.split(","):
            table = table.strip()
            if table not in TABLES:
                raise ValueError(f"Modality ({table}) is invalid.")

    print("Exporting database to SQLite.")
    filepath = f"{settings.artifact_dir}/tomcat.db"

    skip = None
    if args.include == "all":
        tables = "--all"
        skip = " ".join(
            [f"--skip={table.strip()}" for table in args.exclude.split(",")]
        )
    else:
        tables = " ".join(
            [f"--table={table.strip()}" for table in args.include.split(",")]
        )
        skip = None

    if skip:
        command = (
            f"db-to-sqlite -p {tables} {skip} {SQLALCHEMY_DATABASE_URI} {filepath}"
        )
    else:
        command = f"db-to-sqlite -p {tables} {SQLALCHEMY_DATABASE_URI} {filepath}"

    answer = input(
        "The following command will be executed. Do you want to proceed? (y/n): "
    )
    if answer.lower() in ["y", "yes"]:
        pass
        # if subprocess.call(command, shell=True) == 0:
        #     print(f"Database to successfully exported to SQLite. Saved in {SQLITE_DB_PATH}")
        # else:
        #     print("Could not export database to SQLite.")
    else:
        print("Operation aborted.")
