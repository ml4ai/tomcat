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

TASKS = (
    "rest_state",
    "finger_tapping",
    "affective_individual",
    "affective_team",
    "ping_pong_competitive_0",
    "ping_pong_competitive_1",
    "ping_pong_cooperative",
    "saturn_a",
    "saturn_b",
)

MODALITIES = (
    "eeg",
    "fnirs",
    "gaze",
)

STATIONS = (
    "lion",
    "tiger",
    "leopard",
)


def recreate_station_table(db_connection):
    db_connection.execute("DROP TABLE IF EXISTS station")
    db_connection.execute(
        """
        CREATE TABLE station (
            id TEXT PRIMARY KEY
        );"""
    )
    db_connection.executemany(
        "INSERT INTO station VALUES(?)",
        [(station,) for station in STATIONS],
    )


def recreate_modality_table(db_connection):
    db_connection.execute("DROP TABLE IF EXISTS modality")
    db_connection.execute(
        """
        CREATE TABLE modality (
            id TEXT PRIMARY KEY
        );"""
    )

    db_connection.executemany(
        "INSERT INTO modality VALUES(?)",
        [(modality,) for modality in MODALITIES],
    )


def recreate_participant_table(db_connection):
    db_connection.execute("DROP TABLE IF EXISTS participant")
    db_connection.execute(
        """
        CREATE TABLE participant (
            id INTEGER PRIMARY KEY
        );"""
    )


def recreate_group_session_table(db_connection):
    db_connection.execute("DROP TABLE IF EXISTS group_session")
    db_connection.execute(
        """
        CREATE TABLE group_session (
            id TEXT PRIMARY KEY,
            notes TEXT
        );"""
    )


def recreate_data_validity_table(db_connection):
    db_connection.execute("DROP TABLE IF EXISTS data_validity")
    db_connection.execute(
        """
        CREATE TABLE data_validity (
            group_session TEXT,
            participant INTEGER,
            station TEXT,
            task TEXT,
            modality TEXT,
            is_valid BOOLEAN DEFAULT TRUE,
            FOREIGN KEY(group_session) REFERENCES group_session(id)
            FOREIGN KEY(participant) REFERENCES participant(id)
            FOREIGN KEY(station) REFERENCES station(id)
            FOREIGN KEY(task) REFERENCES task(id)
            FOREIGN KEY(modality) REFERENCES modality(id)
        );"""
    )


def insert_values(
    db_connection,
    table_name: str,
    column_name_fragment: str,
    group_session_id: str,
    participants,
    series,
):

    db_connection.execute(
        f"""
        INSERT INTO {table_name}
        VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    """,
        (
            group_session_id,
            participants[0],
            participants[1],
            participants[2],
            series[f"lion_eeg_data_{column_name_fragment}"] == "ok",
            series[f"tiger_eeg_data_{column_name_fragment}"] == "ok",
            series[f"leopard_eeg_data_{column_name_fragment}"] == "ok",
            series[f"lion_nirs_data_{column_name_fragment}"] == "ok",
            series[f"tiger_nirs_data_{column_name_fragment}"] == "ok",
            series[f"leopard_nirs_data_{column_name_fragment}"] == "ok",
            series[f"lion_pupil_data_{column_name_fragment}"] == "ok",
            series[f"tiger_pupil_data_{column_name_fragment}"] == "ok",
            series[f"leopard_pupil_data_{column_name_fragment}"] == "ok",
        ),
    )


def process_rick_workbook():
    """Process Rick's CSVs"""

    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        db_connection.execute("DROP TABLE IF EXISTS task")
        db_connection.execute(
            """
            CREATE TABLE task (
                id TEXT PRIMARY KEY
            );"""
        )
        db_connection.executemany(
            "INSERT INTO task VALUES(?)",
            [(task,) for task in TASKS],
        )

        recreate_station_table(db_connection)
        recreate_modality_table(db_connection)
        recreate_participant_table(db_connection)
        recreate_group_session_table(db_connection)
        recreate_data_validity_table(db_connection)

        # TODO Integrate the 'mask_on' statuses.

    csv_path = (
        "/space/adarsh/tomcat/rick_csvs/view_exp_face_screen_all_crosstab_updated.csv"
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
                [(participant,) for participant in participants],
            )

            # For the group session on 2022-09-30, confederate with ID 99901
            # filled in for participant 00012 during the Saturn A mission,
            # since participant 00012 got motion sickness and had to quit the
            # experiment.
            if group_session_id == "exp_2022_09_30_10":
                db_connection.execute(
                    "INSERT OR IGNORE into participant VALUES(?)", (99901,)
                )

            db_connection.execute(
                "INSERT into group_session VALUES(?, ?)",
                (group_session_id, None),
            )

            # TODO: Deal with 'no_face_image' case for eeg data.
            for station in STATIONS:
                for modality in MODALITIES:
                    modality_in_csv = modality.replace("gaze", "pupil")
                    for task in TASKS:
                        task_in_csv = (
                            task.replace(
                                "affective_individual",
                                "affective_task_individual",
                            )
                            .replace("affective_team", "affective_task_team")
                            .replace(
                                "ping_pong_cooperative",
                                "ping_pong_cooperative_0",
                            )
                        )
                        if task_in_csv == "finger_tapping":
                            participant_id = series[f"{station}_subject_id"]
                        else:
                            participant_id = series[f"{station}_{task_in_csv}_participant_id"]
                        if participant_id == "mission_not_run":
                            continue;


                        db_connection.execute(
                            f"""
                            INSERT INTO data_validity
                            VALUES(?, ?, ?, ?, ?, ?)""",
                            (
                                group_session_id,
                                participant_id,
                                station,
                                task,
                                modality,
                                series[
                                    f"{station}_{modality_in_csv}_data_{task_in_csv}"
                                ]
                                == "ok",
                            ),
                        )


if __name__ == "__main__":
    process_rick_workbook()
