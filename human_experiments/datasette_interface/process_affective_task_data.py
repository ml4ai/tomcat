#!/usr/bin/env python

import json
import logging
import math
import os
import sys
from glob import glob
from logging import info, debug

import pandas as pd
import pyxdf
from sqlalchemy.orm import Session
from tqdm import tqdm

from config import USER
from entity.task.affective_task_event import AffectiveTaskEvent
from entity.base.data_validity import DataValidity
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
            filename=f"/space/{USER}/tomcat/build_affective_task_data_table.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def process_directory_v1(group_session, participants):
    affective_task_events = []
    with cd(f"{group_session}/baseline_tasks/affective"):
        info("Processing individual affective task files.")
        individual_csv_files = glob("individual_*.csv")
        team_csv_files = glob("team_*.csv")

        # We expect exactly 3 CSV file.
        assert len(individual_csv_files) == 3
        assert len(team_csv_files) == 1

        # Get nominal participant IDs from CSV file names:
        nominal_participants = {
            int(filename.split("_")[1]) for filename in individual_csv_files
        }
        debug(f"Nominal participants: {nominal_participants}")

        for csv_file in individual_csv_files:
            nominal_participant_id = int(csv_file.split("_")[1])

            if group_session == "exp_2022_11_04_10":
                info(
                    "[CORRECTION]: For the affective task in the exp_2022_11_04_10"
                    " session, the CSV files have participant IDs 00052, 00058, and"
                    " 99999. However, Rick's experiment tracker CSV has 99906 at"
                    " the 'lion' station, '00053' at the 'tiger' station, and 00052"
                    " at the 'leopard' station. It was later identified that"
                    " the 00058 for Tiger written on the clipboard was a typo,"
                    " it should have been 00053. Experimenter 99999 in this"
                    " case corresponds to experimenter 99906."
                )

                if nominal_participant_id == 99999:
                    real_participant_id = 99906
                elif nominal_participant_id == 58:
                    real_participant_id = 53
                elif nominal_participant_id == 52:
                    real_participant_id = 52
                else:
                    raise ValueError(
                        f"Bad nominal participant ID: {nominal_participant_id}"
                    )

            else:
                real_participant_id = (
                    nominal_participant_id
                    if nominal_participant_id in set(participants.values())
                    else list(
                        set(participants.values()) - nominal_participants
                    )[0]
                )

            df = pd.read_csv(
                csv_file,
                delimiter=";",
                usecols=[
                    "time",
                    "image_path",
                    "arousal_score",
                    "valence_score",
                    "event_type",
                ],
                dtype={
                    "time": str,
                    "image_path": str,
                    "subject_id": str,
                    "arousal_score": str,
                    "valence_score": str,
                },
            )

            for i, row in df.iterrows():
                if pd.isna(row["image_path"]):
                    image_path = None
                else:
                    image_path = row["image_path"]

                if pd.isna(row["arousal_score"]):
                    arousal = None
                else:
                    arousal = int(row["arousal_score"])

                if pd.isna(row["valence_score"]):
                    valence = None
                else:
                    valence = int(row["valence_score"])

                affective_task_event = AffectiveTaskEvent(
                    group_session_id=group_session,
                    participant_id=real_participant_id,
                    task_type="individual",
                    timestamp_unix=row["time"],
                    timestamp_iso8601=convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                    event_type=row["event_type"],
                    image_path=image_path,
                    arousal_score=arousal,
                    valence_score=valence
                )
                affective_task_events.append(affective_task_event)

        for csv_file in team_csv_files:
            df = pd.read_csv(
                csv_file,
                delimiter=";",
                usecols=[
                    "time",
                    "image_path",
                    "subject_id",
                    "arousal_score",
                    "valence_score",
                    "event_type",
                ],
                dtype={
                    "time": str,
                    "image_path": str,
                    "subject_id": str,
                    "arousal_score": str,
                    "valence_score": str,
                },
            )

            for i, row in df.iterrows():
                if pd.isna(row["subject_id"]):
                    real_participant_id = -2
                else:
                    real_participant_id = (
                        int(row["subject_id"])
                        if int(row["subject_id"]) in set(participants.values())
                        else list(
                            set(participants.values()) - nominal_participants
                        )[0]
                    )

                if pd.isna(row["image_path"]):
                    image_path = None
                else:
                    image_path = row["image_path"]

                if pd.isna(row["arousal_score"]):
                    arousal = None
                else:
                    arousal = int(row["arousal_score"])

                if pd.isna(row["valence_score"]):
                    valence = None
                else:
                    valence = int(row["valence_score"])

                affective_task_event = AffectiveTaskEvent(
                    group_session_id=group_session,
                    participant_id=real_participant_id,
                    task_type="team",
                    timestamp_unix=str(row["time"]),
                    timestamp_iso8601=convert_unix_timestamp_to_iso8601(df["time"].iloc[i]),
                    event_type=row["event_type"],
                    image_path=image_path,
                    arousal_score=arousal,
                    valence_score=valence
                )
                affective_task_events.append(affective_task_event)

    return affective_task_events


def process_directory_v2(group_session, participants):
    """Process directory assuming unified XDF files."""

    affective_task_events = []
    with cd(f"{group_session}/lsl"):
        streams, header = pyxdf.load_xdf(
            "block_1.xdf", select_streams=[{"type": "affective_task"}]
        )
        nominal_participant_to_stream_map = {}

        for i, stream in enumerate(streams):
            stream_name = stream["info"]["name"][0]
            stream_source = stream_name.split("_")[-1]

            task_type = "individual" if i != 3 else "team"

            for timestamp, data in zip(
                    stream["time_stamps"], stream["time_series"]
            ):
                data = json.loads(data[0])

                nominal_participant_id = data["subject_id"]

                if task_type == "individual":
                    real_participant_id = participants[stream_source]
                    if (
                            nominal_participant_id
                            not in nominal_participant_to_stream_map
                    ):
                        nominal_participant_to_stream_map[
                            nominal_participant_id
                        ] = stream_source

                elif task_type == "team":
                    if nominal_participant_id is None:
                        real_participant_id = -2
                    else:
                        real_participant_id = participants[
                            nominal_participant_to_stream_map[
                                nominal_participant_id
                            ]
                        ]

                affective_task_event = AffectiveTaskEvent(
                    group_session_id=group_session,
                    participant_id=real_participant_id,
                    task_type=task_type,
                    timestamp_unix=timestamp,
                    timestamp_iso8601=convert_unix_timestamp_to_iso8601(timestamp),
                    event_type=data["event_type"],
                    image_path=data["image_path"],
                    arousal_score=data["arousal_score"],
                    valence_score=data["valence_score"]
                )
                affective_task_events.append(affective_task_event)

    return affective_task_events


def process_affective_task_data(database_engine):
    info(
        """
        Processing affective task data. For the CSV files predating the
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

        with Session(database_engine) as database_session:
            for group_session in tqdm(
                    sorted(directories_to_process), unit="directories"
            ):
                info(f"Processing directory {group_session}")
                # Get real participant IDs for the task
                participants = {}
                for station in ["lion", "tiger", "leopard"]:
                    participant = database_session.query(DataValidity.participant_id).filter(
                        DataValidity.group_session_id == group_session,
                        DataValidity.task_id.like("affective%"),
                        DataValidity.station_id == station).first()[0]
                    participants[station] = participant
                if not is_directory_with_unified_xdf_files(group_session):
                    affective_task_events = process_directory_v1(group_session, participants)
                else:
                    affective_task_events = process_directory_v2(group_session, participants)

                if len(affective_task_events) > 0:
                    database_session.add_all(affective_task_events)
                    database_session.commit()


def recreate_affective_task_event_tables(database_engine):
    AffectiveTaskEvent.__table__.drop(database_engine, checkfirst=True)
    AffectiveTaskEvent.__table__.create(database_engine, checkfirst=True)
