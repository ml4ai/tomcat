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


logging.basicConfig(
    level=logging.INFO,
    handlers=logging_handlers,
)


def process_fnirs_data():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        info("Dropping fnirs table")
        db_connection.execute("DROP TABLE IF EXISTS fnirs_raw")
        db_connection.execute(
            """
            CREATE TABLE fnirs_raw (
                timestamp_unix TEXT NOT NULL,
                timestamp_iso8601 TEXT NOT NULL,
                participant_id TEXT NOT NULL,
                group_session_id TEXT NOT NULL,
                block TEXT,
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
                FOREIGN KEY(participant_id) REFERENCES participant(id)
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


def insert_data_into_table(stream, participant_id, session, db_connection):
    block = None
    data = [
        [
            timestamp,
            convert_unix_timestamp_to_iso8601(timestamp),
            participant_id,
            session,
            block,
            *list(map(str, stream["time_series"][i][41:])),
        ]
        for i, timestamp in enumerate(stream["time_stamps"])
    ]
    with db_connection:
        query = (
            "INSERT into fnirs_raw VALUES(?, ?, ?, ?, ?, "
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
        for directory in ("lion", "tiger", "leopard"):
            # Get participant id
            participant_id = None
            with db_connection:
                participant_id = db_connection.execute(
                    f"SELECT {directory}_participant_id FROM group_session WHERE group_session.id = '{session}'"
                ).fetchone()[0]

            if "99999" in participant_id:
                info(
                    f"Participant ID '{participant_id}' is a confederate, so we skip processing their fNIRS data."
                )
                continue

            xdf_file = (
                f"{directory}/eeg_fnirs_pupil/{directory}_eeg_fnirs_pupil.xdf"
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
            insert_data_into_table(
                stream, participant_id, session, db_connection
            )


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
                # Get participant id
                participant_id = None
                imac_name = stream["info"]["name"][0].split("_")[0]
                with db_connection:
                    participant_id = db_connection.execute(
                        f"SELECT {imac_name}_participant_id FROM group_session WHERE group_session.id = '{session}'"
                    ).fetchone()[0]

                if "99999" in participant_id:
                    info(
                        f"Participant ID '{participant_id}' is a confederate, so we skip processing their fNIRS data."
                    )
                    continue

                insert_data_into_table(
                    stream, participant_id, session, db_connection
                )


if __name__ == "__main__":
    process_fnirs_data()
