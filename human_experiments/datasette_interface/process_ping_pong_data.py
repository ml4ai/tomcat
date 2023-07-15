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
            filename="/space/adarsh/tomcat/build_ping_pong_competitive_task_table.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def recreate_table():
    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1")
        info("Dropping ping_pong_task_event table.")

        db_connection.execute(
            "DROP TABLE IF EXISTS ping_pong_competitive_task_observation"
        )
        db_connection.execute(
            """
            CREATE TABLE ping_pong_competitive_task_observation (
                group_session TEXT NOT NULL,
                stations TEXT NOT NULL,
                player_1_id INTEGER NOT NULL,
                player_2_id INTEGER NOT NULL,
                timestamp_unix TEXT NOT NULL,
                timestamp_iso8601 TEXT NOT NULL,
                task_started BOOLEAN NOT NULL,
                countdown_timer INTEGER NOT NULL CHECK(countdown_timer >= 0),
                ball_position_x INTEGER NOT NULL CHECK(ball_position_x >= 0),
                ball_position_y INTEGER NOT NULL CHECK(ball_position_y >= 0),
                player_1_paddle_position_x INTEGER NOT NULL CHECK(player_1_paddle_position_x >= 0),
                player_1_paddle_position_y INTEGER NOT NULL CHECK(player_1_paddle_position_y >= 0),
                player_2_paddle_position_x INTEGER NOT NULL CHECK(player_2_paddle_position_x >= 0),
                player_2_paddle_position_y INTEGER NOT NULL CHECK(player_2_paddle_position_y >= 0),
                player_1_score INTEGER NOT NULL CHECK(player_1_score >= 0),
                player_2_score INTEGER NOT NULL CHECK(player_2_score >= 0),
                FOREIGN KEY(group_session) REFERENCES group_session(id)
            );"""
        )


def process_competitive_csv_files(competitive_csv_files, session, participants, db_connection):
    for csv_file in competitive_csv_files:
        df = pd.read_csv(
            csv_file,
            delimiter=";",
            dtype=str,
        )

        left_station, right_station = (
            ["lion", "tiger"]
            if "_0" in csv_file
            else ["leopard", "cheetah"]
        )

        stations = "_".join([left_station, right_station])

        with db_connection:
            player_1_id = participants[left_station]
            # Player 2 is an experimenter on cheetah, but we don't know
            # which experimenter.
            player_2_id = (
                participants[right_station]
                if right_station != "cheetah"
                else -3
            )

            db_connection.execute("PRAGMA foreign_keys = 1")
            for i, row in df.iterrows():
                current_started_value = row["started"]

                seconds = int(row["seconds"])
                # For some reason, pygame sometimes outputs a negative value for seconds
                # elapsed - in this case, the baseline task program writes a
                # value of '110' for the countdown timer. We have seen this occur
                # so far whenever the value in the `started` column changes, for
                # the first value after this change occurs.
                # will replace such values with 120 (the initial value).
                if i != 0:
                    previous_started_value = df.loc[i - 1]["started"]
                    if (
                        current_started_value != previous_started_value
                    ) and (seconds == 110):
                        seconds = 120

                (
                    ball_x,
                    ball_y,
                    player_1_x,
                    player_1_y,
                    player_2_x,
                    player_2_y,
                ) = row.iloc[-7:-1]

                row = (
                    session,
                    stations,
                    player_1_id,
                    player_2_id,
                    row["time"],
                    convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                    current_started_value,
                    seconds,
                    ball_x,
                    ball_y,
                    player_1_x,
                    player_1_y,
                    player_2_x,
                    player_2_y,
                    row["score_left"],
                    row["score_right"],
                )

                try:
                    qmarks = ",".join(["?" for _ in range(len(row))])
                    db_connection.execute(
                        f"""INSERT into ping_pong_competitive_task_observation
                        VALUES({qmarks})""",
                        row,
                    )
                except sqlite3.IntegrityError as e:
                    raise sqlite3.IntegrityError(
                        f"Unable to insert row: {row}! Error: {e}"
                    )
                except sqlite3.InterfaceError as e:
                    raise sqlite3.InterfaceError(
                        f"Unable to insert row: {row}! Error: {e}"
                    )

def process_directory_v1(session, participants, db_connection):
    info(f"Processing directory {session}")
    info(f"Participants from data_validity table: {participants}")
    with cd(f"{session}/baseline_tasks/ping_pong"):
        info("Processing ping pong task files.")
        competitive_csv_files = glob("competitive*.csv")
        assert len(competitive_csv_files) == 2
        process_competitive_csv_files(competitive_csv_files, session, participants, db_connection)
        competitive_csv_files = glob("cooperative*.csv")



def process_directory_v2(session, participants, db_connection):
    """Process directory assuming unified XDF files."""
    # TODO: Implement mapping to real participant IDs.
    info(f"Processing directory {session}")

    with cd(f"{session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"type": "ping_pong"}]
        )
        stream = streams[0]
        # start_timestamp_lsl = stream["time_stamps"][0]
        # stop_timestamp_lsl = stream["time_stamps"][-1]

        for i, (timestamp, data) in enumerate(
            zip(stream["time_stamps"], stream["time_series"])
        ):
            data = json.loads(data[0])
            current_event_type = data["event_type"]

            countdown_timer = data["countdown_timer"]
            if countdown_timer is not None:
                countdown_timer = int(countdown_timer)

            # For some reason, pygame sometimes outputs a negative value for seconds
            # elapsed - in this case, the baseline task program writes a
            # value of '1' for the countdown timer. We have seen this occur
            # so far whenever the value in the `event_type` changes, for
            # the first value after this change occurs.
            # will replace such values with 10 (the initial value).
            if i != 0:
                previous_event_type = json.loads(
                    stream["time_series"][i - 1][0]
                )["event_type"]
                if (current_event_type != previous_event_type) and (
                    countdown_timer == 1
                ):
                    countdown_timer = 10

            lion_value, tiger_value, leopard_value = list(data.values())[-3:]
            row = (
                session,
                data["time"],
                convert_unix_timestamp_to_iso8601(data["time"]),
                current_event_type,
                countdown_timer,
                lion_value,
                tiger_value,
                leopard_value,
            )

            with db_connection:
                db_connection.execute("PRAGMA foreign_keys = 1")
                try:
                    db_connection.execute(
                        "INSERT into ping_pong_task_event VALUES(?, ?, ?, ?, ?, ?, ?, ?);",
                        row,
                    )
                except sqlite3.IntegrityError as e:
                    raise sqlite3.IntegrityError(
                        f"Unable to insert row: {row}! Error: {e}"
                    )


def process_ping_pong_task_data():
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
                participants = {}
                for station in ["lion", "tiger", "leopard"]:
                    participant = db_connection.execute(
                        f"""
                        SELECT DISTINCT(participant) from data_validity
                        WHERE group_session = '{session}' AND task LIKE 'ping%'
                        AND station = '{station}'
                        """
                    ).fetchall()[0][0]
                    participants[station] = participant
            if not is_directory_with_unified_xdf_files(session):
                process_directory_v1(session, participants, db_connection)
            else:
                # process_directory_v2(session, participants, db_connection)
                pass


if __name__ == "__main__":
    info(
        """
        Processing ping pong task data. For the CSV files predating the
        unified XDF file era, We will use the `time` column in the CSV,"
        ignoring the `monotonic_time` and `human_readable` time columns,
        since those timestamps are systematically a few microseconds later
        than the timestamps in the `time` column, since they are created by
        separate invocations to monotonic() and datetime.utcnow() respectively.
        """
    )
    recreate_table()
    process_ping_pong_task_data()
