#!/usr/bin/env python

import json
import logging
import os
import sys
from glob import glob
from logging import info

import pandas as pd
import pyxdf
from tqdm import tqdm

from datasette_interface.common.config import LOG_DIR, settings
from datasette_interface.common.utils import (
    cd,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
    should_ignore_directory,
)
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.task.finger_tapping_task_observation import (
    FingerTappingTaskObservation,
)

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/build_finger_tapping_task_table.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_directory_v1(group_session):
    finger_tapping_observations = []
    with cd(f"{group_session}/baseline_tasks/finger_tapping"):
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
            countdown_timer = None if pd.isna(countdown_timer) else int(countdown_timer)

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
            lion_value = None if pd.isna(lion_value) else int(lion_value)
            tiger_value = None if pd.isna(tiger_value) else int(tiger_value)
            leopard_value = None if pd.isna(leopard_value) else int(leopard_value)

            finger_tapping_observation = FingerTappingTaskObservation(
                group_session_id=group_session,
                timestamp_unix=row["time"],
                timestamp_iso8601=convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                event_type=row["event_type"],
                countdown_timer=countdown_timer,
                lion_spacebar_pressed=lion_value,
                tiger_spacebar_pressed=tiger_value,
                leopard_spacebar_pressed=leopard_value,
            )
            finger_tapping_observations.append(finger_tapping_observation)

    return finger_tapping_observations


def process_directory_v2(group_session):
    """Process directory assuming unified XDF files."""

    finger_tapping_observations = []
    with cd(f"{group_session}/lsl"):
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
                previous_event_type = json.loads(stream["time_series"][i - 1][0])[
                    "event_type"
                ]
                if (current_event_type != previous_event_type) and (
                    countdown_timer == 1
                ):
                    countdown_timer = 10

            lion_value, tiger_value, leopard_value = list(data.values())[-3:]

            finger_tapping_observation = FingerTappingTaskObservation(
                group_session_id=group_session,
                timestamp_unix=data["time"],
                timestamp_iso8601=convert_unix_timestamp_to_iso8601(data["time"]),
                event_type=current_event_type,
                countdown_timer=countdown_timer,
                lion_spacebar_pressed=lion_value,
                tiger_spacebar_pressed=tiger_value,
                leopard_spacebar_pressed=leopard_value,
            )
            finger_tapping_observations.append(finger_tapping_observation)

    return finger_tapping_observations


def process_finger_tapping_task_data():
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

    with cd(settings.experiment_root_dir):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        finger_tapping_observations = []

        db_session = next(get_db())
        processed_group_sessions = set(
            [
                s[0]
                for s in db_session.query(FingerTappingTaskObservation.group_session_id)
                .distinct(FingerTappingTaskObservation.group_session_id)
                .all()
            ]
        )

        for group_session in tqdm(sorted(directories_to_process), unit="directories"):
            if group_session in processed_group_sessions:
                info(
                    f"Found saved finger tapping for {group_session} in the database. "
                    f"Skipping group session."
                )
                continue

            if not is_directory_with_unified_xdf_files(group_session):
                finger_tapping_observations.extend(process_directory_v1(group_session))
            else:
                finger_tapping_observations.extend(process_directory_v2(group_session))

        info("Adding finger tapping task observations to the database.")
        db_session.add_all(finger_tapping_observations)
        db_session.commit()
        db_session.close()
