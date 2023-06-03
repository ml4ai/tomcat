#!/usr/bin/env python

import os
import sys
import sqlite3
from utils import (
    cd,
    should_ignore_directory,
    logging_handlers,
    convert_unix_timestamp_to_iso8601,
)
import pyxdf
import logging
from logging import info
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
        db_connection.execute("DROP TABLE IF EXISTS fnirs")
        db_connection.execute(
            """
            CREATE TABLE fnirs (
                unix_timestamp TEXT NOT NULL,
                iso8601_timestamp TEXT NOT NULL,
                participant_id TEXT NOT NULL,
                group_session_id TEXT NOT NULL,
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
            process_directory_v1(session, db_connection)


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

            xdf_file = (
                f"{directory}/eeg_fnirs_pupil/{directory}_eeg_fnirs_pupil.xdf"
            )
            streams, header = pyxdf.load_xdf(
                xdf_file, select_streams=[{"type": "NIRS"}]
            )
            fnirs_stream = streams[0]
            data = [
                [
                    timestamp,
                    convert_unix_timestamp_to_iso8601(timestamp),
                    participant_id,
                    session,
                    *list(map(str, fnirs_stream["time_series"][i][41:])),
                ]
                for i, timestamp in enumerate(fnirs_stream["time_stamps"][0:2])
            ]
            with db_connection:
                query = (
                    "INSERT into fnirs VALUES(?, ?, ?, ?, "
                    + ",".join(
                        [
                            "?"
                            for i in streams[0]["info"]["desc"][0]["channels"][
                                0
                            ]["channel"][41:]
                        ]
                    )
                    + ")"
                )
                db_connection.executemany(query, data)


if __name__ == "__main__":
    process_fnirs_data()
