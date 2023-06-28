#!/usr/bin/env python

import sys
from dataclasses import dataclass
import logging
from logging import info, warning, error, debug
import sqlite3
import csv
import pandas as pd
from dbml_sqlite import toSQLite
from pprint import pprint
from utils import cd
from config import DB_PATH, logging_handlers


logging.basicConfig(
    level=logging.INFO,
    handlers=logging_handlers,
)


def process_rick_workbook():
    """Process Rick's CSVs"""

    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        db_connection.execute("DROP TABLE IF EXISTS participant")
        db_connection.execute(
            """
            CREATE TABLE participant (
                id INTEGER PRIMARY KEY
            );"""
        )

        db_connection.execute("DROP TABLE IF EXISTS group_session")
        db_connection.execute(
            """
            CREATE TABLE group_session (
                id TEXT PRIMARY KEY
            );"""
        )

        # TODO Integrate the 'mask_on' statuses.
        db_connection.execute("DROP TABLE IF EXISTS rest_state_task_metadata")
        db_connection.execute(
            """
            CREATE TABLE rest_state_task_metadata (
                group_session_id TEXT PRIMARY KEY,
                lion_participant_id INTEGER NOT NULL,
                tiger_participant_id INTEGER NOT NULL,
                leopard_participant_id INTEGER NOT NULL,
                lion_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                lion_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                lion_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                FOREIGN KEY(group_session_id) REFERENCES group_session(id),
                FOREIGN KEY(lion_participant_id) REFERENCES participant(id),
                FOREIGN KEY(tiger_participant_id) REFERENCES participant(id),
                FOREIGN KEY(leopard_participant_id) REFERENCES participant(id)
            );"""
        )

        db_connection.execute("DROP TABLE IF EXISTS finger_tapping_task_metadata")
        db_connection.execute(
            """
            CREATE TABLE finger_tapping_task_metadata (
                group_session_id TEXT PRIMARY KEY,
                lion_participant_id INTEGER NOT NULL,
                tiger_participant_id INTEGER NOT NULL,
                leopard_participant_id INTEGER NOT NULL,
                lion_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                lion_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                lion_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                FOREIGN KEY(group_session_id) REFERENCES group_session(id),
                FOREIGN KEY(lion_participant_id) REFERENCES participant(id),
                FOREIGN KEY(tiger_participant_id) REFERENCES participant(id),
                FOREIGN KEY(leopard_participant_id) REFERENCES participant(id)
            );"""
        )

        db_connection.execute("DROP TABLE IF EXISTS affective_task_individual_metadata")
        db_connection.execute(
            """
            CREATE TABLE affective_task_individual_metadata (
                group_session_id TEXT PRIMARY KEY,
                lion_participant_id INTEGER NOT NULL,
                tiger_participant_id INTEGER NOT NULL,
                leopard_participant_id INTEGER NOT NULL,
                lion_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                lion_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                lion_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                FOREIGN KEY(group_session_id) REFERENCES group_session(id),
                FOREIGN KEY(lion_participant_id) REFERENCES participant(id),
                FOREIGN KEY(tiger_participant_id) REFERENCES participant(id),
                FOREIGN KEY(leopard_participant_id) REFERENCES participant(id)
            );"""
        )

        db_connection.execute("DROP TABLE IF EXISTS affective_task_team_metadata")
        db_connection.execute(
            """
            CREATE TABLE affective_task_team_metadata (
                group_session_id TEXT PRIMARY KEY,
                lion_participant_id INTEGER NOT NULL,
                tiger_participant_id INTEGER NOT NULL,
                leopard_participant_id INTEGER NOT NULL,
                lion_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_eeg_data_is_valid BOOLEAN DEFAULT TRUE,
                lion_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_fnirs_data_is_valid BOOLEAN DEFAULT TRUE,
                lion_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                tiger_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                leopard_gaze_data_is_valid BOOLEAN DEFAULT TRUE,
                FOREIGN KEY(group_session_id) REFERENCES group_session(id),
                FOREIGN KEY(lion_participant_id) REFERENCES participant(id),
                FOREIGN KEY(tiger_participant_id) REFERENCES participant(id),
                FOREIGN KEY(leopard_participant_id) REFERENCES participant(id)
            );"""
        )

    csv_path = (
        "/space/adarsh/tomcat/rick_csvs/view_exp_face_screen_all_crosstab.csv"
    )

    df = pd.read_csv(csv_path, index_col="experiment_id", dtype=str)

    for group_session_id, series in df.iterrows():
        if "canceled" in series["lion_subject_id"]:
            continue

        with db_connection:
            db_connection.execute("PRAGMA foreign_keys = 1")
            participants = [
                series["lion_subject_id"],
                series["tiger_subject_id"],
                series["leopard_subject_id"],
            ]

            db_connection.executemany(
                "INSERT OR IGNORE into participant VALUES(?)",
                [(participant,) for participant in participants]
            )

            db_connection.execute(
                "INSERT into group_session VALUES(?)",
                (group_session_id,),
            )

            # TODO: Deal with 'no_face_image' case for eeg data.
            db_connection.execute(
                """
                INSERT INTO rest_state_task_metadata
                VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
                (
                    group_session_id,
                    participants[0],
                    participants[1],
                    participants[2],
                    series["lion_eeg_data_rest_state"] == "ok",
                    series["tiger_eeg_data_rest_state"] == "ok",
                    series["leopard_eeg_data_rest_state"] == "ok",
                    series["lion_nirs_data_rest_state"] == "ok",
                    series["tiger_nirs_data_rest_state"] == "ok",
                    series["leopard_nirs_data_rest_state"] == "ok",
                    series["lion_pupil_data_rest_state"] == "ok",
                    series["tiger_pupil_data_rest_state"] == "ok",
                    series["leopard_pupil_data_rest_state"] == "ok",
                ),
            )

            db_connection.execute(
                """
                INSERT INTO finger_tapping_task_metadata
                VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
                (
                    group_session_id,
                    participants[0],
                    participants[1],
                    participants[2],
                    series["lion_eeg_data_finger_tapping"] == "ok",
                    series["tiger_eeg_data_finger_tapping"] == "ok",
                    series["leopard_eeg_data_finger_tapping"] == "ok",
                    series["lion_nirs_data_finger_tapping"] == "ok",
                    series["tiger_nirs_data_finger_tapping"] == "ok",
                    series["leopard_nirs_data_finger_tapping"] == "ok",
                    series["lion_pupil_data_finger_tapping"] == "ok",
                    series["tiger_pupil_data_finger_tapping"] == "ok",
                    series["leopard_pupil_data_finger_tapping"] == "ok",
                ),
            )

            db_connection.execute(
                """
                INSERT INTO affective_task_individual_metadata
                VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
                (
                    group_session_id,
                    participants[0],
                    participants[1],
                    participants[2],
                    series["lion_eeg_data_affective_task_individual"] == "ok",
                    series["tiger_eeg_data_affective_task_individual"] == "ok",
                    series["leopard_eeg_data_affective_task_individual"] == "ok",
                    series["lion_nirs_data_affective_task_individual"] == "ok",
                    series["tiger_nirs_data_affective_task_individual"] == "ok",
                    series["leopard_nirs_data_affective_task_individual"] == "ok",
                    series["lion_pupil_data_affective_task_individual"] == "ok",
                    series["tiger_pupil_data_affective_task_individual"] == "ok",
                    series["leopard_pupil_data_affective_task_individual"] == "ok",
                ),
            )

            db_connection.execute(
                """
                INSERT INTO affective_task_team_metadata
                VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
                (
                    group_session_id,
                    participants[0],
                    participants[1],
                    participants[2],
                    series["lion_eeg_data_affective_task_team"] == "ok",
                    series["tiger_eeg_data_affective_task_team"] == "ok",
                    series["leopard_eeg_data_affective_task_team"] == "ok",
                    series["lion_nirs_data_affective_task_team"] == "ok",
                    series["tiger_nirs_data_affective_task_team"] == "ok",
                    series["leopard_nirs_data_affective_task_team"] == "ok",
                    series["lion_pupil_data_affective_task_team"] == "ok",
                    series["tiger_pupil_data_affective_task_team"] == "ok",
                    series["leopard_pupil_data_affective_task_team"] == "ok",
                ),
            )

if __name__ == "__main__":
    process_rick_workbook()
