#!/usr/bin/env python

import json
import logging
import os
import sys
from glob import glob
from logging import info

import pandas as pd
import pyxdf
from sqlalchemy.orm import Session
from tqdm import tqdm

from config import USER
from entity.task.finger_tapping_task_observation import FingerTappingTaskObservation
from utils import (
    cd,
    should_ignore_directory,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
)

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_fingertapping_task_table.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_directory_v1(group_session_id):

    finger_tapping_observations = []
    with cd(f"{group_session_id}/baseline_tasks/finger_tapping"):
        csv_files = glob("*.csv")
        # We expect exactly 1 CSV file.
        assert len(csv_files) == 1
        csv_file = csv_files[0]

        df = pd.read_csv(
            csv_file,
            delimiter=";",
            dtype=str,
        )

        for i, row in df.iterrows():
            current_event_type = row["event_type"]

            countdown_timer = row["countdown_timer"]
            if not pd.isna(countdown_timer):
                countdown_timer = int(countdown_timer)

            # For some reason, pygame sometimes outputs a negative value for seconds
            # elapsed - in this case, the baseline task program writes a
            # value of '1' for the countdown timer. We have seen this occur
            # so far whenever the value in the `event_type` changes, for
            # the first value after this change occurs.
            # will replace such values with 10 (the initial value).
            if i != 0:
                previous_event_type = df.loc[i - 1]["event_type"]
                if (current_event_type != previous_event_type) and (
                    countdown_timer == 1
                ):
                    countdown_timer = 10

            lion_value, tiger_value, leopard_value = row.iloc[-3:]

            finger_tapping_observation = FingerTappingTaskObservation(
                group_session_id=group_session_id,
                timestamp_unix=row["time"],
                timestamp_iso8601=convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                event_type=row["event_type"],
                countdown_timer=countdown_timer,
                lion_spacebar_pressed=lion_value,
                tiger_spacebar_pressed=tiger_value,
                leopard_spacebar_pressed=leopard_value
            )
            finger_tapping_observations.append(finger_tapping_observation)

    return finger_tapping_observations


def process_directory_v2(group_session_id):
    """Process directory assuming unified XDF files."""

    finger_tapping_observations = []
    with cd(f"{group_session_id}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"type": "finger_tapping"}]
        )
        stream = streams[0]

        for i, (timestamp, data) in enumerate(
            zip(stream["time_stamps"], stream["time_series"])
        ):
            data = json.loads(data[0])
            current_event_type = data["event_type"]

            countdown_timer = data["countdown_timer"]
            if countdown_timer is not None:
                countdown_timer = int(countdown_timer)

            # For some reason, pygame sometimes outputs a negative value for seconds
            # elapsed - in this case, the baseline task program writes a
            # value of '1' for the countdown timer. We have seen this occur
            # so far whenever the value in the `event_type` changes, for
            # the first value after this change occurs.
            # will replace such values with 10 (the initial value).
            if i != 0:
                previous_event_type = json.loads(
                    stream["time_series"][i - 1][0]
                )["event_type"]
                if (current_event_type != previous_event_type) and (
                    countdown_timer == 1
                ):
                    countdown_timer = 10

            lion_value, tiger_value, leopard_value = list(data.values())[-3:]

            finger_tapping_observation = FingerTappingTaskObservation(
                group_session_id=group_session_id,
                timestamp_unix=data["time"],
                timestamp_iso8601=convert_unix_timestamp_to_iso8601(data["time"]),
                event_type=current_event_type,
                countdown_timer=countdown_timer,
                lion_spacebar_pressed=lion_value,
                tiger_spacebar_pressed=tiger_value,
                leopard_spacebar_pressed=leopard_value
            )
            finger_tapping_observations.append(finger_tapping_observation)

    return finger_tapping_observations


def process_finger_tapping_task_data(database_engine):
    info(
        """
        Processing finger tapping task data. For the CSV files predating the
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
        for session in tqdm(
            sorted(directories_to_process), unit="directories"
        ):
            if not is_directory_with_unified_xdf_files(session):
                finger_tapping_observations.extend(process_directory_v1(session))
            else:
                finger_tapping_observations.extend(process_directory_v2(session))

        info("Adding finger tapping task observations to the database.")
        with Session(database_engine) as database_session:
            database_session.add_all(finger_tapping_observations)
            database_session.commit()


def recreate_finger_tapping_task_observation_table(database_engine):
    FingerTappingTaskObservation.__table__.drop(database_engine, checkfirst=True)
    FingerTappingTaskObservation.__table__.create(database_engine, checkfirst=True)
