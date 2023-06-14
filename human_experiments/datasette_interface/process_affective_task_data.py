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
import json


logging.basicConfig(
    level=logging.INFO,
    handlers=logging_handlers,
)


def process_directory_v1(session, db_connection):
    info(f"Processing directory {session}")
    with cd(f"{session}/baseline_tasks/affective"):
        info("Processing individual affective task files.")
        individual_csv_files = glob("individual_*.csv")
        team_csv_files = glob("team_*.csv")
        csv_files = {
            "individual": individual_csv_files,
            "team": team_csv_files,
        }

        # We expect exactly 3 CSV file.
        assert len(individual_csv_files) == 3
        assert len(team_csv_files) == 1

        for csv_file in individual_csv_files:
            participant_id = csv_file.split("_")[1]
            df = pd.read_csv(
                csv_file,
                delimiter=";",
                usecols=[
                    "time",
                    "image_path",
                    "subject_id",
                    "arousal_score",
                    "valence_score",
                    "event_type",
                ],
                dtype={
                    "time": str,
                    "image_path": str,
                    "subject_id": str,
                    "arousal_score": str,
                    "valence_score": str,
                    }
            )

            rows = [
                (
                    session,
                    participant_id,
                    "individual",
                    row["time"],
                    convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                    row["event_type"],
                    row["image_path"],
                    row["arousal_score"],
                    row["valence_score"],
                )
                for i, row in df.iterrows()
            ]

            with db_connection:
                db_connection.executemany(
                    "INSERT into affective_task_event VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?);",
                    rows,
                )

        for csv_file in team_csv_files:
            df = pd.read_csv(
                csv_file,
                delimiter=";",
                usecols=[
                    "time",
                    "image_path",
                    "subject_id",
                    "arousal_score",
                    "valence_score",
                    "event_type",
                ],
                dtype={
                    "time": str,
                    "image_path": str,
                    "subject_id": str,
                    "arousal_score": str,
                    "valence_score": str,
                    }
            )

            rows = [
                (
                    session,
                    row["subject_id"],
                    "team",
                    row["time"],
                    convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                    row["event_type"],
                    row["image_path"],
                    row["arousal_score"],
                    row["valence_score"],
                )
                for i, row in df.iterrows()
            ]

            with db_connection:
                db_connection.executemany(
                    "INSERT into affective_task_event VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?);",
                    rows,
                )


def process_directory_v2(session, db_connection):
    """Process directory assuming unified XDF files."""
    info(f"Processing directory {session}")
    with cd(f"{session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"type": "affective_task"}]
        )
        for i, stream in enumerate(streams):
            # start_timestamp_lsl = stream["time_stamps"][0]
            # stop_timestamp_lsl = stream["time_stamps"][-1]

            task_type = "individual" if i != 3 else "team"
            rows = [
                (
                    session,
                    json.loads(data[0])["subject_id"],
                    task_type,
                    timestamp,
                    convert_unix_timestamp_to_iso8601(timestamp),
                    json.loads(data[0])["event_type"],
                    json.loads(data[0])["image_path"],
                    json.loads(data[0])["arousal_score"],
                    json.loads(data[0])["valence_score"],
                )
                for timestamp, data in zip(
                    stream["time_stamps"], stream["time_series"]
                )
            ]

            with db_connection:
                db_connection.executemany(
                    "INSERT into affective_task_event VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?);",
                    rows,
                )


def process_rest_state_task_data():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        info("Dropping affective_task_event table")
        db_connection.execute(
            "DROP TABLE IF EXISTS individual_affective_task_event"
        )

        db_connection.execute("DROP TABLE IF EXISTS affective_task_event")
        db_connection.execute(
            """
            CREATE TABLE affective_task_event (
                group_session_id TEXT NOT NULL,
                participant_id TEXT,
                task_type TEXT NOT NULL,
                timestamp_unix TEXT NOT NULL,
                timestamp_iso8601 TEXT NOT NULL,
                event_type TEXT NOT NULL,
                image_path TEXT,
                arousal_score INTEGER,
                valence_score INTEGER,
                FOREIGN KEY(group_session_id) REFERENCES group_session(id)
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
