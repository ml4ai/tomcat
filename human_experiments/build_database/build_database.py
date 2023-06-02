#!/usr/bin/env python

import contextlib
import os
import sys
import json
from dataclasses import dataclass
from glob import glob
import logging
from logging import info, warning, error
from tqdm import tqdm
import sqlite3
import csv
import pandas as pd
from dbml_sqlite import toSQLite

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
                    mission = message["data"]["experiment_mission"]
                    trial_start_timestamp = message["header"]["timestamp"]
                    testbed_version = message["data"]["testbed_version"]
                elif message["msg"]["sub_type"] == "stop":
                    trial_stop_timestamp = message["header"]["timestamp"]

            if topic == "observations/events/scoreboard":
                score = message["data"]["scoreboard"]["TeamScore"]
                scores.append(score)

    if len(scores) != 0:
        warning(f"\t\tNo scoreboard messages found!")
        final_team_score = scores[-1]

    data = (
            trial_uuid,
            session,
            mission,
            testbed_version,
            final_team_score,
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
    else:
        return False


def process_directory(session, db_connection):
    info(f"Processing directory {session}")
    try:
        with cd(f"{session}/minecraft"):
            metadata_files = sorted(glob("*.metadata"))
            for metadata_file in metadata_files:
                info(f"\tProcessing file {metadata_file}")
                process_metadata_file(
                    metadata_file, session, db_connection
                )

    except FileNotFoundError:
        warning(f"minecraft directory not in {session}")


def initialize_database():
    schema = toSQLite("schema.dbml")
    with sqlite3.connect(DB_PATH) as db_connection:
        db_connection.executescript(schema)


def process_directories():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        for session in tqdm(sorted(os.listdir("."))):
            if should_ignore_directory(session):
                continue
            else:
                process_directory(session, db_connection)


def process_rick_workbook():
    """Process Rick's Excel workbook."""
    db_connection = sqlite3.connect(DB_PATH)
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
                (group_session_id, *participants)
            )



if __name__ == "__main__":
    initialize_database()
    process_rick_workbook()
    process_directories()
