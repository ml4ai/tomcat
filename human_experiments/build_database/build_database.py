#!/usr/bin/env python

import contextlib
import os
import sys
import json
from dataclasses import dataclass
from glob import glob
import logging
from logging import info, warning, error, debug
from tqdm import tqdm
import sqlite3
import csv
import pandas as pd
from dbml_sqlite import toSQLite
from pprint import pprint

LOG_FILE_PATH = "/space/adarsh/tomcat/build_database.log"
DB_PATH = "/space/adarsh/tomcat/test.db"
FILE_HANDLER = logging.FileHandler(filename=LOG_FILE_PATH, mode="w")
STDERR_HANDLER = logging.StreamHandler(stream=sys.stderr)
handlers = [FILE_HANDLER, STDERR_HANDLER]

logging.basicConfig(
    level=logging.INFO,
    handlers=handlers,
)


@contextlib.contextmanager
def cd(path):
    old_path = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(old_path)


def process_metadata_file(filepath, session, db_connection):
    trial_uuid = None
    mission = None
    trial_start_timestamp = None
    trial_stop_timestamp = None
    testbed_version = None
    final_team_score = None
    scores = []

    with open(filepath) as f:
        for line in f:
            message = json.loads(line)
            topic = message["topic"]
            if topic == "trial":
                trial_uuid = message["msg"]["trial_id"]
                if message["msg"]["sub_type"] == "start":
                    trial_start_timestamp = message["header"]["timestamp"]
                    mission = message["data"]["experiment_mission"]
                    testbed_version = message["data"]["testbed_version"]
                elif message["msg"]["sub_type"] == "stop":
                    trial_stop_timestamp = message["header"]["timestamp"]

            if topic == "observations/events/scoreboard":
                score = message["data"]["scoreboard"]["TeamScore"]
                scores.append(score)

    if len(scores) != 0:
        error(f"[MISSING DATA]:\t\tNo scoreboard messages found!")
        final_team_score = scores[-1]

    data = (
        trial_uuid,
        session,
        mission,
        final_team_score,
        testbed_version,
    )

    try:
        with db_connection:
            db_connection.execute("PRAGMA foreign_keys = 1")
            db_connection.execute(
                "INSERT into mission VALUES(?, ?, ?, ?, ?)", data
            )
    except sqlite3.IntegrityError:
        raise sqlite3.IntegrityError(f"Unable to insert row: {data}")


def should_ignore_directory(session) -> bool:
    """Returns true if this directory should be ignored."""
    year, month, day, hour = [int(x) for x in session.split("_")[1:]]

    if year == 2022 and ((month < 9) or (month == 9 and day < 30)):
        info(
            f"Ignoring {session} since our first pilot with real "
            "participants was on 9/30/2022"
        )
        return True
    elif session == "exp_2023_04_20_14":
        info(
            f"Ignoring {session}. Since only one participant showed up, the session was cancelled."
        )
        return True

    elif session == "exp_2023_02_20_13":
        info(
            f"Ignoring {session}, since it is a duplicate of the "
            "exp_2023_02_20_01 directory."
        )
        return True

    elif session in {"exp_2022_12_05_15", "exp_2023_04_26_10"}:
        info(
            f"Ignoring {session}, since it was cancelled (no participants showed up.)"
        )
        return True
    elif (year, month) >= (2023, 4):
        info(
            f"Ignoring {session}, since we have not implemented processing the new unified XDF files yet."
        )
        return True
    else:
        return False


def collect_files_to_process(metadata_files):
    missions = {}
    # all_key_message_types = {"trial_start", "trial_stop"}
    for metadata_file in metadata_files:
        # key_messages = {"trial_start": []}
        with open(metadata_file) as f:
            debug(f"Inspecting {metadata_file} to get key messages.")
            for line in f:
                message = json.loads(line)
                topic = message["topic"]
                if topic == "trial":
                    if message["msg"]["sub_type"] == "start":
                        trial_start_timestamp = message["header"]["timestamp"]
                        mission = message["data"]["experiment_mission"]
                        if mission not in missions:
                            missions[mission] = {metadata_file: trial_start_timestamp}
                        else:
                            missions[mission][metadata_file] = trial_start_timestamp
                        break

            # Check whether we have all the key messages (trial start/stop, mission start/stop)

            # for message_type, messages in key_messages.items():
                # if len(messages) != 1:
                    # error(f"{len(messages)} {message_type} messages found!")
                    # for message in messages:
                        # debug(
                            # f"Timestamp: {message['header']['timestamp']}"
                            # + f"\ttrial_id: {message['msg']['trial_id']}"
                        # )

            # missing_key_messages = all_key_message_types - set(key_messages)
            # if len(missing_key_messages) != 0:
            # # if missing_key_messages == {"mission_stop"}:
            # warning(f"Missing key messages: {missing_key_messages}.")


    for mission, files in missions.items():
        files_to_ignore = []
        if len(files) > 1:
            info(
                f"More than one .metadata file matches the {mission} mission."
            )
            for file, timestamp in sorted(files.items()):
                print(f"{timestamp}\t{file}")

            files_sorted_by_timestamp_in_descending_order = [
                file
                for (file, timestamp) in sorted(
                    files.items(), key=lambda x: x[1], reverse=True
                )
            ]

            most_recent_file = files_sorted_by_timestamp_in_descending_order[0]
            files_to_ignore.extend(
                files_sorted_by_timestamp_in_descending_order[1:]
            )

            info(
                "We will select the most recent one by inspecting the .header.timestamp"
                " value of the first trial start message in the .metadata file."
                f"\nMost recent .metadata file: {most_recent_file}"
            )

        for file in files_to_ignore:
            del files[file]

    all_missions = {"Hands-on Training", "Saturn_A", "Saturn_B"}
    missing_missions = all_missions - set(missions.keys())
    if len(missing_missions) != 0:
        info(f"\tMissing missions: {missing_missions}")

    files_to_process = [list(x[1].keys())[0] for x in [item for item in missions.items()]]
    return files_to_process


def process_directory_v1(session, db_connection):
    """Process directory assuming it is from before we had the unified XDF files."""

    info(f"Processing directory {session}")
    try:
        with cd(f"{session}/minecraft"):
            metadata_files = sorted(glob("*.metadata"))
            files_to_process = collect_files_to_process(metadata_files)
            for metadata_file in files_to_process:
                info(f"\tProcessing {metadata_file}")
                process_metadata_file(metadata_file, session, db_connection)
    except FileNotFoundError:
        error(f"[MISSING DATA]: No 'minecraft' directory found in {session}. Skipping the directory!")

def initialize_database():
    schema = toSQLite("schema.dbml")
    with sqlite3.connect(DB_PATH) as db_connection:
        db_connection.executescript(schema)


def process_directories():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        db_connection.execute("DROP TABLE IF EXISTS mission")
        db_connection.execute(
            """
            CREATE TABLE mission (
                id TEXT PRIMARY KEY,
                group_session_id TEXT,
                name TEXT,
                final_team_score TEXT,
                testbed_version TEXT,
                FOREIGN KEY(group_session_id) REFERENCES group_session(id)
            );"""
        )

    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        for session in tqdm(sorted(directories_to_process), unit = "directories"):
            if should_ignore_directory(session):
                continue
            else:
                process_directory_v1(session, db_connection)


def process_rick_workbook():
    """Process Rick's Excel workbook."""

    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        db_connection.execute("DROP TABLE IF EXISTS participant")
        db_connection.execute(
            """
            CREATE TABLE participant (
                id TEXT PRIMARY KEY,
                is_confederate INTEGER DEFAULT False
            );"""
        )

        db_connection.execute("DROP TABLE IF EXISTS group_session")
        db_connection.execute(
            """
            CREATE TABLE group_session (
                id TEXT PRIMARY KEY,
                lion_participant_id TEXT NOT NULL,
                tiger_participant_id TEXT NOT NULL,
                leopard_participant_id TEXT NOT NULL,
                FOREIGN KEY(lion_participant_id) REFERENCES participant(id),
                FOREIGN KEY(tiger_participant_id) REFERENCES participant(id),
                FOREIGN KEY(leopard_participant_id) REFERENCES participant(id)
            );"""
        )

    workbook_path = "/tomcat/data/raw/rick_experiment_tracker.xlsx"

    df = pd.read_excel(workbook_path, skiprows=1, index_col="Experiment ID")

    for group_session_id, series in df.iterrows():
        if series["Start Date/time"] == "CANCELLED":
            continue

        with db_connection:
            db_connection.execute("PRAGMA foreign_keys = 1")
            participants = [
                series["Lion Subject ID"],
                series["Tiger Subject ID"],
                series["Leopard Subject ID"],
            ]

            db_connection.executemany(
                "INSERT OR IGNORE into participant VALUES(?, ?)",
                [
                    (participant, 1 if "99999" in participant else 0)
                    for participant in participants
                ],
            )

            db_connection.execute(
                "INSERT into group_session VALUES(?, ?, ?, ?)",
                (group_session_id, *participants),
            )


if __name__ == "__main__":
    # initialize_database()
    # process_rick_workbook()
    process_directories()
