#!/usr/bin/env python

import contextlib
import os
import sys
import json
from dataclasses import dataclass
from glob import glob
import logging
from logging import info, warning
from tqdm import tqdm
import sqlite3
import csv


file_handler = logging.FileHandler(filename='build_database.log', mode='w')
stderr_handler = logging.StreamHandler(stream=sys.stderr)
handlers = [file_handler, stderr_handler]

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


def process_metadata_file(filepath, team_id, db_connection):
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

    data = [
        (
            trial_uuid,
            team_id,
            mission,
            trial_start_timestamp,
            trial_stop_timestamp,
            testbed_version,
            final_team_score,
        )
    ]

    # try:
    with db_connection:
        db_connection.executemany(
            "INSERT into trial_info VALUES(?, ?, ?, ?, ?, ?, ?)", data
        )
    # info(f"Inserted row: {data}")
    # except sqlite3.IntegrityError:
        # info(f"Unable to insert row: {data}")


if __name__ == "__main__":
    info("Processing directories...")
    db_connection = sqlite3.connect("test.db")
    with open("schema.sql") as f:
        schema = f.read()

    with db_connection:
        db_connection.executescript(schema)

    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        for session in tqdm(sorted(os.listdir("."))):
            year, month, day, hour = [int(x) for x in session.split("_")[1:]]

            if year == 2022 and ((month < 9) or (month == 9 and day < 30)):
                info(
                    f"Ignoring {session} since our first pilot with real "
                    "participants was on 9/30/2022"
                )
                continue

            elif (year, month, day) == (2023, 4, 20):
                info(
                    f"Ignoring {session}. Since only one participant showed up, the session was cancelled."
                )
                continue

            elif (year, month, day, hour) == (2023, 2, 20, 1):
                info(
                    f"Ignoring {session}, since its data is duplicated in the"
                    "exp_2023_02_20_13 directory."
                )
                continue

            else:
                team_id = None
                with open(f"{session}/redcap_data/team_data.csv") as f:
                    reader = csv.DictReader(f)
                    for i, row in enumerate(reader):
                       if i!=0:
                           raise ValueError("More than one record found in team_data.csv!")
                       team_id = int(row["team_id"])
                       participants = [int(x) for x in row["subject_id"].split(",")]
                       data = [(participant_id, team_id) for participant_id in participants]

                       with db_connection:
                            db_connection.executemany(
                                "INSERT into participant VALUES(?, ?)", data
                            )
                            info(f"Inserted rows: {data}")

                try:
                    with cd(f"{session}/minecraft"):
                        info(f"Processing directory {session}")
                        metadata_files = sorted(glob("*.metadata"))
                        for metadata_file in metadata_files:
                            info(f"\tProcessing file {metadata_file}")
                            process_metadata_file(metadata_file, team_id, db_connection)

                except FileNotFoundError:
                    warning(f"minecraft directory not in {session}")
