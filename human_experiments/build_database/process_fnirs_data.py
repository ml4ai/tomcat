#!/usr/bin/env python

import os
import sys
import sqlite3
from utils import cd, should_ignore_directory, logging_handlers
import pyxdf
import logging
from logging import info
from config import DB_PATH, logging_handlers
from tqdm import tqdm


logging.basicConfig(
    level=logging.ERROR,
    handlers=logging_handlers,
)


def process_fnirs_data():
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        db_connection.execute("DROP TABLE IF EXISTS fnirs")
        db_connection.execute(
            """
            CREATE TABLE fnirs (
                participant_id TEXT NOT NULL,
                group_session_id TEXT NOT NULL,
                S1-D1_HbO REAL NOT NULL,
                S1-D2_HbO REAL NOT NULL,
                S2-D1_HbO REAL NOT NULL,
                S2-D3_HbO REAL NOT NULL,
                S3-D1_HbO REAL NOT NULL,
                S3-D3_HbO REAL NOT NULL,
                S3-D4_HbO REAL NOT NULL,
                S4-D2_HbO REAL NOT NULL,
                S4-D4_HbO REAL NOT NULL,
                S4-D5_HbO REAL NOT NULL,
                S5-D3_HbO REAL NOT NULL,
                S5-D4_HbO REAL NOT NULL,
                S5-D6_HbO REAL NOT NULL,
                S6-D4_HbO REAL NOT NULL,
                S6-D6_HbO REAL NOT NULL,
                S6-D7_HbO REAL NOT NULL,
                S7-D5_HbO REAL NOT NULL,
                S7-D7_HbO REAL NOT NULL,
                S8-D6_HbO REAL NOT NULL,
                S8-D7_HbO REAL NOT NULL,
                S1-D1_HbR REAL NOT NULL,
                S1-D2_HbR REAL NOT NULL,
                S2-D1_HbR REAL NOT NULL,
                S2-D3_HbR REAL NOT NULL,
                S3-D1_HbR REAL NOT NULL,
                S3-D3_HbR REAL NOT NULL,
                S3-D4_HbR REAL NOT NULL,
                S4-D2_HbR REAL NOT NULL,
                S4-D4_HbR REAL NOT NULL,
                S4-D5_HbR REAL NOT NULL,
                S5-D3_HbR REAL NOT NULL,
                S5-D4_HbR REAL NOT NULL,
                S5-D6_HbR REAL NOT NULL,
                S6-D4_HbR REAL NOT NULL,
                S6-D6_HbR REAL NOT NULL,
                S6-D7_HbR REAL NOT NULL,
                S7-D5_HbR REAL NOT NULL,
                S7-D7_HbR REAL NOT NULL,
                S8-D6_HbR REAL NOT NULL,
                S8-D7_HbR REAL NOT NULL,
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

            xdf_file = f"{directory}/eeg_fnirs_pupil/{directory}_eeg_fnirs_pupil.xdf"
            streams, header = pyxdf.load_xdf(xdf_file, select_streams = [{"type": "NIRS"}])
            for i in range(3):
                with db_connection:
                    db_connection.execute(
                       f"INSERT into fnirs VALUES(
                ) 
                S1-D2_HbO REAL NOT NULL,
                S2-D1_HbO REAL NOT NULL,
                S2-D3_HbO REAL NOT NULL,
                S3-D1_HbO REAL NOT NULL,
                S3-D3_HbO REAL NOT NULL,
                S3-D4_HbO REAL NOT NULL,
                S4-D2_HbO REAL NOT NULL,
                S4-D4_HbO REAL NOT NULL,
                S4-D5_HbO REAL NOT NULL,
                S5-D3_HbO REAL NOT NULL,
                S5-D4_HbO REAL NOT NULL,
                S5-D6_HbO REAL NOT NULL,
                S6-D4_HbO REAL NOT NULL,
                S6-D6_HbO REAL NOT NULL,
                S6-D7_HbO REAL NOT NULL,
                S7-D5_HbO REAL NOT NULL,
                S7-D7_HbO REAL NOT NULL,
                S8-D6_HbO REAL NOT NULL,
                S8-D7_HbO REAL NOT NULL,
                S1-D1_HbR REAL NOT NULL,
                S1-D2_HbR REAL NOT NULL,
                S2-D1_HbR REAL NOT NULL,
                S2-D3_HbR REAL NOT NULL,
                S3-D1_HbR REAL NOT NULL,
                S3-D3_HbR REAL NOT NULL,
                S3-D4_HbR REAL NOT NULL,
                S4-D2_HbR REAL NOT NULL,
                S4-D4_HbR REAL NOT NULL,
                S4-D5_HbR REAL NOT NULL,
                S5-D3_HbR REAL NOT NULL,
                S5-D4_HbR REAL NOT NULL,
                S5-D6_HbR REAL NOT NULL,
                S6-D4_HbR REAL NOT NULL,
                S6-D6_HbR REAL NOT NULL,
                S6-D7_HbR REAL NOT NULL,
                S7-D5_HbR REAL NOT NULL,
                S7-D7_HbR REAL NOT NULL,
                S8-D6_HbR REAL NOT NULL,
                S8-D7_HbR REAL NOT NULL,

                       )
                    )

if __name__ == "__main__":
    process_fnirs_data()
