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
from tqdm.contrib.concurrent import process_map


logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename="/space/adarsh/tomcat/build_fnirs_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def recreate_fnirs_table():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")
        info("Dropping fnirs table")
        db_connection.execute("DROP TABLE IF EXISTS fnirs_raw")
        db_connection.execute(
            """
            CREATE TABLE fnirs_raw (
                group_session TEXT NOT NULL,
                task TEXT,
                station TEXT NOT NULL,
                participant TEXT NOT NULL,
                timestamp_unix TEXT NOT NULL,
                timestamp_iso8601 TEXT NOT NULL,
                S1_D1_HbO REAL NOT NULL,
                S1_D2_HbO REAL NOT NULL,
                S2_D1_HbO REAL NOT NULL,
                S2_D3_HbO REAL NOT NULL,
                S3_D1_HbO REAL NOT NULL,
                S3_D3_HbO REAL NOT NULL,
                S3_D4_HbO REAL NOT NULL,
                S4_D2_HbO REAL NOT NULL,
                S4_D4_HbO REAL NOT NULL,
                S4_D5_HbO REAL NOT NULL,
                S5_D3_HbO REAL NOT NULL,
                S5_D4_HbO REAL NOT NULL,
                S5_D6_HbO REAL NOT NULL,
                S6_D4_HbO REAL NOT NULL,
                S6_D6_HbO REAL NOT NULL,
                S6_D7_HbO REAL NOT NULL,
                S7_D5_HbO REAL NOT NULL,
                S7_D7_HbO REAL NOT NULL,
                S8_D6_HbO REAL NOT NULL,
                S8_D7_HbO REAL NOT NULL,
                S1_D1_HbR REAL NOT NULL,
                S1_D2_HbR REAL NOT NULL,
                S2_D1_HbR REAL NOT NULL,
                S2_D3_HbR REAL NOT NULL,
                S3_D1_HbR REAL NOT NULL,
                S3_D3_HbR REAL NOT NULL,
                S3_D4_HbR REAL NOT NULL,
                S4_D2_HbR REAL NOT NULL,
                S4_D4_HbR REAL NOT NULL,
                S4_D5_HbR REAL NOT NULL,
                S5_D3_HbR REAL NOT NULL,
                S5_D4_HbR REAL NOT NULL,
                S5_D6_HbR REAL NOT NULL,
                S6_D4_HbR REAL NOT NULL,
                S6_D6_HbR REAL NOT NULL,
                S6_D7_HbR REAL NOT NULL,
                S7_D5_HbR REAL NOT NULL,
                S7_D7_HbR REAL NOT NULL,
                S8_D6_HbR REAL NOT NULL,
                S8_D7_HbR REAL NOT NULL,
                FOREIGN KEY(group_session) REFERENCES group_session(id)
                FOREIGN KEY(task) REFERENCES task(id)
                FOREIGN KEY(station) REFERENCES station(id)
                FOREIGN KEY(participant) REFERENCES participant(id)
            );"""
        )


def insert_raw_unlabeled_data():
    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        db_connection = sqlite3.connect(DB_PATH)
        with db_connection:
            db_connection.execute("PRAGMA foreign_keys = 1;")
            for session in tqdm(
                sorted(directories_to_process), unit="directories"
            ):
                if not is_directory_with_unified_xdf_files(session):
                    process_directory_v1(session, db_connection)
                else:
                    process_directory_v2(session, db_connection)


def insert_data_into_table(stream, session, station, db_connection):
    # We insert a participant ID of -1 since we don't actually know for sure
    # who the participant is - we will need to consult the data validity table
    # to learn the ID, since the originally scheduled participant might be
    # replaced by an experimenter partway through the group session.
    task = None
    participant_id = -1
    data = [
        [
            session,
            task,
            station,
            participant_id,
            timestamp,
            convert_unix_timestamp_to_iso8601(timestamp),
            *list(map(str, stream["time_series"][i][41:])),
        ]
        for i, timestamp in enumerate(stream["time_stamps"])
    ]

    with db_connection:
        query = (
            "INSERT into fnirs_raw VALUES(?, ?, ?, ?, ?, ?,"
            + ",".join(
                [
                    "?"
                    for i in stream["info"]["desc"][0]["channels"][0][
                        "channel"
                    ][41:]
                ]
            )
            + ")"
        )
        db_connection.executemany(query, data)


def process_directory_v1(session, db_connection):
    info(f"Processing directory {session}")
    with cd(f"{session}"):
        for station in ("lion", "tiger", "leopard"):
            xdf_file = (
                f"{station}/eeg_fnirs_pupil/{station}_eeg_fnirs_pupil.xdf"
            )
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"type": "NIRS"}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No fNIRS stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            stream = streams[0]
            insert_data_into_table(stream, session, station, db_connection)


def process_directory_v2(session, db_connection):
    """Process directory assuming unified XDF files."""
    info(f"Processing directory {session}")
    with cd(f"{session}/lsl"):
        for xdf_file in ("block_1.xdf", "block_2.xdf"):
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"type": "NIRS"}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No fNIRS stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            for stream in streams:
                station = stream["info"]["name"][0].split("_")[0]
                insert_data_into_table(stream, session, station, db_connection)

def create_index():
    """Create index for efficient querying"""
    info("Creating index for fnirs_raw table.")
    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        db_connection.execute("""
            CREATE INDEX IF NOT EXISTS idx_group_session_station
            ON fnirs_raw (group_session, station);
        """)

def label_data():
    # label rest state task data
    # 1. Get the rows corresponding to TASK in GROUP_SESSION for STATION, by
    # looking at the timestamps in the rest_state_task table.
    # 2. Check the data_validity table to see if the fNIRS data for TASK,
    # GROUP_SESSION, STATION is VALID.
    # 3. If the data is not valid, remove the rows.
    # 4. If the data is valid, label the rows with the participant ID.
    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:

        validity_rows = db_connection.execute(f"""
            SELECT * from data_validity
            WHERE task='rest_state'
            AND modality='fnirs';
        """).fetchall()

        for row in validity_rows:
            group_session, participant_id, station, task , modality, is_valid = row
            rowids = [x[0] for x in db_connection.execute(f"""
                SELECT rowid from fnirs_raw
                WHERE group_session='{group_session}'
                AND station='{station}';
            """).fetchall()]

            start_timestamp, stop_timestamp = db_connection.execute(f"""
                SELECT start_timestamp_unix, stop_timestamp_unix from rest_state_task
                WHERE group_session_id='{group_session}';
            """).fetchall()[0]

            db_connection.execute(f"""
            UPDATE fnirs_raw
            SET
            participant_id='{participant_id}',
            """)





if __name__ == "__main__":
    info("Starting building fNIRS table.")
    # recreate_fnirs_table()
    # insert_raw_unlabeled_data()
    # create_index()
    label_data()
    info("Finished building fNIRS table.")
