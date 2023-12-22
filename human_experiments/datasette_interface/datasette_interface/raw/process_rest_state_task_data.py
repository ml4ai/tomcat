#!/usr/bin/env python

import logging
import os
import sys
from glob import glob
from logging import info, error

import pandas as pd
import pyxdf
from sqlalchemy.orm import Session
from tqdm import tqdm

from datasette_interface.common.config import USER
from datasette_interface.database.entity.task.rest_state_task import RestStateTask
from utils import (
    cd,
    should_ignore_directory,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
)
from datasette_interface.database.config import get_db, engine
from datasette_interface.common.config import LOG_DIR, settings

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/build_rest_state_task_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_directory_v1(group_session):
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


def process_rest_state_task_data():
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

    with cd(settings.experiment_root_dir):
        directories_to_process = [
            directory
            for directory in os.listdir("../..")
            if not should_ignore_directory(directory)
        ]

        db_session = next(get_db())
        processed_group_sessions = set(
            [s[0] for s in db_session.query(RestStateTask.group_session_id).distinct(
                RestStateTask.group_session_id).all()])

        for group_session in tqdm(sorted(directories_to_process), unit="directories"):
            if group_session in processed_group_sessions:
                info(f"Found saved rest state data for {group_session} in the database. "
                     f"Skipping group session.")
                continue

            info(f"Processing directory {group_session}")
            if not is_directory_with_unified_xdf_files(group_session):
                rest_state_entry = process_directory_v1(group_session)
            else:
                rest_state_entry = process_directory_v2(group_session)

            if rest_state_entry:
                db_session.add(rest_state_entry)
                db_session.commit()
        db_session.close()
