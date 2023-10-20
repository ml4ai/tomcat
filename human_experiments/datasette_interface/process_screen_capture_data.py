#!/usr/bin/env python
"""Script to process screen capture data"""
import json
import logging
import os
import sys
from glob import glob
from logging import info, error, debug
import time
from dateutil import parser

import pyxdf
from sqlalchemy.orm import Session
from tqdm import tqdm

from config import USER
from entity.base.base import Base
from entity.signal.screen_capture import ScreenCapture
from utils import (
    cd,
    should_ignore_directory,
    convert_iso8601_timestamp_to_unix,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
)
from process_raw_signals import get_signals

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_screen_capture_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_directory_v1(group_session):
    image_records = []
    for station in ("lion", "tiger", "leopard"):
        with cd(f"{group_session}/baseline_tasks/screenshots/{station}"):
            timestamp_from_name = False
            if os.path.exists("outFile.csv"):
                # The images were renamed with their creation date, so the timestamp of each image
                # can be retrieved directly from their filename. # Images before 2022-11-07 do not
                # have their creation date saved. In this case we use their last modified date as
                # timestamp, which we can retrieve from the file's metadata.
                timestamp_from_name = True

            unique_id = 0
            for filename in os.listdir("."):
                if filename[filename.rfind(".") + 1:] != "png":
                    # Skip files that are not images.
                    continue

                if timestamp_from_name:
                    timestamp_iso8601 = filename[:filename.rfind(".")]
                    timestamp_unix = convert_iso8601_timestamp_to_unix(timestamp_iso8601)
                else:
                    mod_time = os.path.getmtime(filename)
                    timestamp_unix = parser.parse(time.ctime(mod_time)).timestamp()
                    timestamp_iso8601 = convert_unix_timestamp_to_iso8601(timestamp_unix)

                # Task and participant will be filled later in the labeling part.
                screen_capture = ScreenCapture(
                    group_session_id=group_session,
                    id=unique_id,
                    task_id=None,
                    station_id=station,
                    participant_id=-1,
                    timestamp_unix=timestamp_unix,
                    timestamp_iso8601=timestamp_iso8601,
                    filename=filename
                )
                image_records.append(screen_capture)

                unique_id += 1

    return image_records


def process_directory_v2(group_session):
    """Process directory assuming unified XDF files."""

    records = []
    with cd(f"{group_session}/lsl"):
        max_ids = {}
        for xdf_file in ("block_1.xdf", "block_2.xdf"):
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"name": "Screen"}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No screen capture stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            for stream in streams:
                # For v2, the signals are split in two files (block1 and 2). When processing block2
                # entries, we need to make sure we start the ID taking into account the last ID in
                # the block 1 files for the same station, otherwise we get a duplicate key error
                # during insertion.
                station = stream["info"]["hostname"]
                next_id = max_ids.get(station, - 1) + 1
                station = stream["info"]["hostname"][0]
                tmp = get_signals(stream, group_session, station, next_id, ScreenCapture,
                                  lambda x: ["filename"], slice_series_fn=lambda x: x)
                records.extend(tmp)
                max_ids[station] = tmp[-1].id

    return records


def process_screen_capture_data(database_engine, override):
    info(
        """
        Processing screen capture data. For the CSV files predating the
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

        finger_tapping_observations = []

        with Session(database_engine) as database_session:
            processed_group_sessions = set(
                [s[0] for s in
                 database_session.query(FingerTappingTaskObservation.group_session_id).distinct(
                     FingerTappingTaskObservation.group_session_id).all()])

            for group_session in tqdm(sorted(directories_to_process), unit="directories"):
                if not override and group_session in processed_group_sessions:
                    info(
                        f"Found saved finger tapping for {group_session} in the database. Skipping group session.")
                    continue

                if not is_directory_with_unified_xdf_files(group_session):
                    finger_tapping_observations.extend(process_directory_v1(group_session))
                else:
                    finger_tapping_observations.extend(process_directory_v2(group_session))

            info("Adding finger tapping task observations to the database.")
            database_session.add_all(finger_tapping_observations)
            database_session.commit()


def recreate_finger_tapping_task_observation_tables(database_engine):
    FingerTappingTaskObservation.__table__.drop(database_engine, checkfirst=True)
    FingerTappingTaskObservation.__table__.create(database_engine, checkfirst=True)
