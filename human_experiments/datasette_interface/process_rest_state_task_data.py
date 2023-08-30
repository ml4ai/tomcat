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
from config import DB_PATH, logging_handlers, USER
from tqdm import tqdm
import pandas as pd
from glob import glob
from sqlalchemy.orm import Session

from entity.task.rest_state import RestStateTask

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_rest_state_task_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_directory_v1(group_session):
    info(f"Processing directory {group_session}")

    with cd(f"{group_session}/baseline_tasks/rest_state"):
        csv_files = glob("*.csv")

        # We expect exactly one CSV file.
        assert len(csv_files) == 1

        if os.stat(csv_files[0]).st_size == 0:
            error(
                f"[MISSING DATA]: The size of the CSV file {csv_files[0]} that"
                " should have contained the rest state task timestamps is"
                " zero, so we cannot process it."
            )
            return

        df = pd.read_csv(csv_files[0], delimiter=";")

        start_timestamp_unix = df["time"].iloc[0]
        stop_timestamp_unix = df["time"].iloc[-1]

        return RestStateTask(
            group_session_id=group_session,
            start_timestamp_unix=start_timestamp_unix,
            start_timestamp_iso8601=convert_unix_timestamp_to_iso8601(start_timestamp_unix),
            stop_timestamp_unix=stop_timestamp_unix,
            stop_timestamp_iso8601=convert_unix_timestamp_to_iso8601(stop_timestamp_unix)
        )


def process_directory_v2(group_session):
    """Process directory assuming unified XDF files."""
    info(f"Processing directory {group_session}")

    with cd(f"{group_session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"type": "rest_state"}]
        )
        stream = streams[0]
        start_timestamp_lsl, stop_timestamp_lsl = stream["time_stamps"]

        return RestStateTask(
            group_session_id=group_session,
            start_timestamp_unix=start_timestamp_lsl,
            start_timestamp_iso8601=convert_unix_timestamp_to_iso8601(start_timestamp_lsl),
            stop_timestamp_unix=stop_timestamp_lsl,
            stop_timestamp_iso8601=convert_unix_timestamp_to_iso8601(stop_timestamp_lsl)
        )


def process_rest_state_task_data(database_engine):
    info(
        """
        Processing rest state task data. For the CSV files predating the
        unified XDF file era, We will use the `time` column in the CSV,"
        ignoring the `monotonic_time` and `human_readable` time columns,
        since those timestamps are systematically a few microseconds later
        than the timestamps in the `time` column, since they are created by
        separate invocations to monotonic() and datetime.utcnow() respectively.
        """
    )

    info("Processing directories...")

    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        rest_state_entries = []

        for group_session in tqdm(
            sorted(directories_to_process), unit="directories"
        ):
            if not is_directory_with_unified_xdf_files(group_session):
                rest_state_entry = process_directory_v1(group_session)
            else:
                rest_state_entry = process_directory_v2(group_session)

            if rest_state_entry:
                rest_state_entries.append(rest_state_entry)

        # Insert all entries in bulk
        info("Adding rest state entries to the database.")
        with Session(database_engine) as database_session:
            database_session.add_all(rest_state_entries)
            database_session.commit()


def recreate_rest_state_table(database_engine):
    RestStateTask.__table__.drop(database_engine, checkfirst=True)
    RestStateTask.__table__.create(database_engine, checkfirst=True)
