#!/usr/bin/env python

import os
import sys
import sqlite3
from utils import (
    cd,
    should_ignore_directory,
    logging_handlers,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
)
import pyxdf
import logging
from logging import info, error
from config import DB_PATH, logging_handlers
from tqdm import tqdm
import pandas as pd
from glob import glob

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename="/space/adarsh/tomcat/build_rest_state_task_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)

def process_directory_v1(session, db_connection):
    info(f"Processing directory {session}")
    with cd(f"{session}/baseline_tasks/rest_state"):
        csv_files = glob("*.csv")

        # We expect exactly one CSV file.
        assert len(csv_files) == 1

        if os.stat(csv_files[0]).st_size == 0:
            error(
                f"[MISSING DATA]: The size of the CSV file {csv_files[0]} that"
                " should have contained the rest state task timestamps is"
                " zero, so we cannot process it."
            )
            return

        df = pd.read_csv(csv_files[0], delimiter=";")

        start_timestamp_unix = df["time"].iloc[0]
        stop_timestamp_unix = df["time"].iloc[-1]
        row = (
            session,
            start_timestamp_unix,
            convert_unix_timestamp_to_iso8601(start_timestamp_unix),
            stop_timestamp_unix,
            convert_unix_timestamp_to_iso8601(stop_timestamp_unix),
        )

        with db_connection:
            db_connection.execute(
                "INSERT into rest_state_task VALUES(?, ?, ?, ?, ?);", row
            )


def process_directory_v2(session, db_connection):
    """Process directory assuming unified XDF files."""
    info(f"Processing directory {session}")
    with cd(f"{session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"type": "rest_state"}]
        )
        stream = streams[0]
        start_timestamp_lsl, stop_timestamp_lsl = stream["time_stamps"]

        row = (
            session,
            start_timestamp_lsl,
            convert_unix_timestamp_to_iso8601(start_timestamp_lsl),
            stop_timestamp_lsl,
            convert_unix_timestamp_to_iso8601(stop_timestamp_lsl),
        )

        with db_connection:
            db_connection.execute(
                "INSERT into rest_state_task VALUES(?, ?, ?, ?, ?);", row
            )


def process_rest_state_task_data():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        info("Dropping rest_state_task table")
        db_connection.execute("DROP TABLE IF EXISTS rest_state_task")
        db_connection.execute(
            """
            CREATE TABLE rest_state_task (
                group_session TEXT NOT NULL,
                start_timestamp_unix TEXT NOT NULL,
                start_timestamp_iso8601 TEXT NOT NULL,
                stop_timestamp_unix TEXT NOT NULL,
                stop_timestamp_iso8601 TEXT NOT NULL,
                FOREIGN KEY(group_session) REFERENCES group_session(id)
            );"""
        )

    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        for session in tqdm(
            sorted(directories_to_process), unit="directories"
        ):
            if not is_directory_with_unified_xdf_files(session):
                process_directory_v1(session, db_connection)
            else:
                process_directory_v2(session, db_connection)


if __name__ == "__main__":
    info(
        """
        Processing rest state task data. For the CSV files predating the
        unified XDF file era, We will use the `time` column in the CSV,"
        ignoring the `monotonic_time` and `human_readable` time columns,
        since those timestamps are systematically a few microseconds later
        than the timestamps in the `time` column, since they are created by
        separate invocations to monotonic() and datetime.utcnow() respectively.
        """
    )
    process_rest_state_task_data()
