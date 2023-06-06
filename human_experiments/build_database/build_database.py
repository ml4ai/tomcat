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
from process_minecraft_data import process_minecraft_data
from utils import cd
from config import DB_PATH, logging_handlers


logging.basicConfig(
    level=logging.INFO,
    handlers=logging_handlers,
)

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
    # process_rick_workbook()
    process_minecraft_data()
