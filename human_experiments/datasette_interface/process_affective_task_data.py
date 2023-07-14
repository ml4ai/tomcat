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
    handlers=(
        logging.FileHandler(
            filename="/space/adarsh/tomcat/build_affective_task_data_table.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def recreate_table():
    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1")
        info("Dropping affective_task_event table.")

        db_connection.execute("DROP TABLE IF EXISTS affective_task_event")
        db_connection.execute(
            """
            CREATE TABLE affective_task_event (
                group_session TEXT NOT NULL,
                participant INTEGER,
                task_type TEXT NOT NULL,
                timestamp_unix TEXT NOT NULL,
                timestamp_iso8601 TEXT NOT NULL,
                event_type TEXT NOT NULL,
                image_path TEXT,
                arousal_score INTEGER,
                valence_score INTEGER,
                FOREIGN KEY(group_session) REFERENCES group_session(id)
                FOREIGN KEY(participant) REFERENCES participant(id)
            );"""
        )


def process_directory_v1(session, participants, db_connection):
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

        # Get nominal participant IDs from CSV file names:
        nominal_participants = {
            int(filename.split("_")[1]) for filename in individual_csv_files
        }

        for csv_file in individual_csv_files:
            nominal_participant_id = int(csv_file.split("_")[1])
            real_participant_id = (
                nominal_participant_id
                if nominal_participant_id in participants
                else list(participants - nominal_participants)[0]
            )

            df = pd.read_csv(
                csv_file,
                delimiter=";",
                usecols=[
                    "time",
                    "image_path",
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
                },
            )

            with db_connection:
                db_connection.execute("PRAGMA foreign_keys = 1")
                for i, row in df.iterrows():
                    row = (
                        session,
                        real_participant_id,
                        "individual",
                        row["time"],
                        convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                        row["event_type"],
                        row["image_path"],
                        row["arousal_score"],
                        row["valence_score"],
                    )
                    try:
                        db_connection.execute(
                            "INSERT into affective_task_event VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?);",
                            row,
                        )
                    except sqlite3.IntegrityError as e:
                        raise sqlite3.IntegrityError(
                            f"Unable to insert row: {row}! Error: {e}"
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
                },
            )

            for i, row in df.iterrows():
                if pd.isna(row["subject_id"]):
                    real_participant_id = -2
                else:
                    real_participant_id = (
                        int(row["subject_id"])
                        if int(row["subject_id"]) in participants
                        else list(participants - nominal_participants)[0]
                    )

                row = (
                    session,
                    real_participant_id,
                    "team",
                    row["time"],
                    convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                    row["event_type"],
                    row["image_path"],
                    row["arousal_score"],
                    row["valence_score"],
                )

                with db_connection:
                    try:
                        db_connection.execute(
                            "INSERT into affective_task_event VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?);",
                            row,
                        )
                    except sqlite3.IntegrityError as e:
                        raise sqlite3.IntegrityError(
                            f"Unable to insert row: {row}! Error: {e}"
                        )


def process_directory_v2(session, participants, db_connection):
    """Process directory assuming unified XDF files."""
    # TODO: Implement mapping to real participant IDs.
    info(f"Processing directory {session}")

    with cd(f"{session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"type": "affective_task"}]
        )
        for i, stream in enumerate(streams):
            # start_timestamp_lsl = stream["time_stamps"][0]
            # stop_timestamp_lsl = stream["time_stamps"][-1]

            task_type = "individual" if i != 3 else "team"

            rows = []

            for timestamp, data in zip(
                stream["time_stamps"], stream["time_series"]
            ):
                data = json.loads(data[0])

                nominal_participant_id = (
                    int(data["subject_id"])
                    if data["subject_id"] is not None
                    else -2
                )

                # Replace nominal participant ID with real participant ID.
                real_participant_id = (
                    nominal_participant_id
                    if nominal_participant_id in participants
                    or nominal_participant_id == -2
                    else [
                        participant_id
                        for participant_id in list(participants)
                        if participant_id > 999
                    ][0]
                )

                row = (
                    session,
                    real_participant_id,
                    task_type,
                    timestamp,
                    convert_unix_timestamp_to_iso8601(timestamp),
                    data["event_type"],
                    data["image_path"],
                    data["arousal_score"],
                    data["valence_score"],
                )
                rows.append(row)

            with db_connection:
                db_connection.execute("PRAGMA foreign_keys = 1")
                for row in rows:
                    try:
                        db_connection.execute(
                            "INSERT into affective_task_event VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?);",
                            row,
                        )
                    except sqlite3.IntegrityError as e:
                        raise sqlite3.IntegrityError(
                            f"Unable to insert row: {row}! Error: {e}"
                        )


def process_affective_task_data():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        for session in tqdm(
            sorted(directories_to_process), unit="directories"
        ):
            # Get real participant IDs for the task
            with db_connection:
                db_connection.execute("PRAGMA foreign_keys = 1")
                participants = {
                    row[0]
                    for row in db_connection.execute(
                        f"""
                    SELECT DISTINCT(participant) from data_validity
                    WHERE group_session = '{session}' and task LIKE 'affective%';
                    """
                    ).fetchall()
                }
            if not is_directory_with_unified_xdf_files(session):
                process_directory_v1(session, participants, db_connection)
            else:
                process_directory_v2(session, participants, db_connection)


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
    recreate_table()
    process_affective_task_data()
