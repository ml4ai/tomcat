#!/usr/bin/env python

import logging
import math
import sys
from logging import info

import pandas as pd
from sqlalchemy.orm import Session

from datasette_interface.common.config import USER
from datasette_interface.database.entity.base.base import Base
from datasette_interface.database.entity.base.data_validity import DataValidity
from datasette_interface.database.entity.base.eeg_device import EEGDevice
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.modality import Modality
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.task import Task
from datasette_interface.database.config import get_db, engine
from datasette_interface.common.config import LOG_DIR

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/build_base_tables.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)

TASKS = [
    "rest_state",
    "finger_tapping",
    "affective_individual",
    "affective_team",
    "ping_pong_competitive",
    "ping_pong_cooperative",
    "hands_on_training",
    "saturn_a",
    "saturn_b",
]

MODALITIES = (
    "eeg",
    "fnirs",
    "gaze",
)

STATIONS = [
    "lion",
    "tiger",
    "leopard",
]


def populate_task_table():
    db_session = next(get_db())
    modalities = [Task(id=task) for task in TASKS]
    db_session.add_all(modalities)
    db_session.commit()
    db_session.close()


def populate_station_table():
    db_session = next(get_db())
    stations = [Station(id=station) for station in STATIONS + ["cheetah"]]
    db_session.add_all(stations)
    db_session.commit()
    db_session.close()


def populate_modality_table():
    db_session = next(get_db())
    modalities = [Modality(id=modality) for modality in MODALITIES]
    db_session.add_all(modalities)
    db_session.commit()
    db_session.close()


def populate_participant_table():
    db_session = next(get_db())
    participants = [
        # ID of -1 to represent 'unknown participant'
        Participant(id=-1),
        # ID of -2 to represent the team (for affective task event data)
        Participant(id=-2),
        # ID of - 3 to represent an unknown experimenter(for the ping pong task data)
        Participant(id=-3)
    ]
    db_session.add_all(participants)
    db_session.commit()
    db_session.close()


def populate_base_tables():
    info(f"Populating base tables.")

    populate_task_table()
    populate_station_table()
    populate_modality_table()
    populate_participant_table()


def process_data_validity_workbook():
    info(f"Processing data validity workbook.")

    # TODO Integrate the 'mask_on' statuses.

    csv_path = "/tomcat/data/raw/LangLab/experiments/study_3_pilot/data_validity_table.csv"

    df = pd.read_csv(csv_path, index_col="experiment_id", dtype=str)

    for group_session_id, series in df.iterrows():
        group_session_id = str(group_session_id)

        if "canceled" in series["lion_subject_id"]:
            continue

        participants = []

        db_session = next(get_db())
        for prefix in ["lion", "tiger", "leopard"]:
            participant_id = series[f"{prefix}_subject_id"]
            if not db_session.query(Participant.id).filter_by(id=participant_id).first():
                # Only add participant if it does not exist in the table. SQLAlchemy does not have a DBMS-agnostic
                # treatment for this (e.g. INSERT IGNORE) so the way to do it is to checking if the PK exists in the
                # table before inserting the entry into it.
                participants.append(Participant(id=participant_id))

        # For the group session on 2022-09-30, confederate with ID 99901
        # filled in for participant 00012 during the Saturn A mission,
        # since participant 00012 got motion sickness and had to quit the
        # experiment.
        if group_session_id == "exp_2022_09_30_10":
            participant_id = 99901
            if not db_session.query(Participant.id).filter_by(id=participant_id).first():
                participants.append(Participant(id=participant_id))

        # TODO: Deal with 'no_face_image' case for eeg data.
        data_validity_entries = []
        tasks_new = TASKS + ["ping_pong_competitive_0", "ping_pong_competitive_1"]
        tasks_new = [task for task in tasks_new if task != "ping_pong_competitive"]
        for station in STATIONS:
            for modality in MODALITIES:
                modality_in_csv = modality.replace("gaze", "pupil")
                for task in tasks_new:
                    if (
                            (task == "ping_pong_competitive_1")
                            and (station in {"lion", "tiger"})
                    ) or (
                            (task == "ping_pong_competitive_0")
                            and (station == "leopard")
                    ):
                        info(f"""
                            [ERROR]: Task = {task} and station = {station},
                            a combination that is not possible.
                            ping_pong_competitive_0 was performed on the
                            'lion' and 'tiger' stations, while
                            'ping_pong_competitive_1' was performed on the
                            'leopard' and 'cheetah' stations. The 'cheetah'
                            station was manned by an experimenter (but we
                            did not record who the experimenter was).
                            We will skip entering this combination in the
                            data_validity table.
                        """)
                        continue

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
                    participant_id = series[
                        f"{station}_{task_in_csv}_participant_id"
                    ]

                    if participant_id == "mission_not_run":
                        continue

                    # Remove the _0, _1 suffixes from the names of the
                    # ping pong competitive tasks before we write them to
                    # the database.
                    task = task.replace("_0", "").replace("_1", "")

                    is_valid = series[f"{station}_{modality_in_csv}_data_{task_in_csv}"] == "ok"
                    data_validity = DataValidity(
                        group_session_id=group_session_id,
                        participant_id=participant_id,
                        station_id=station,
                        task_id=task,
                        modality_id=modality,
                        is_valid=is_valid
                    )

                    data_validity_entries.append(data_validity)

        db_session.add(GroupSession(id=group_session_id))
        db_session.add_all(participants)
        # Flush to communicate changes to the database in a pending state such that we don't encounter
        # foreign key errors when inserting data validity entries.
        db_session.flush()

        db_session.add_all(data_validity_entries)
        db_session.commit()
        db_session.close()


def process_station_to_eeg_amp_mapping_workbook():
    info(f"Process station to eeg amp mapping workbook.")

    csv_path = "/tomcat/data/raw/LangLab/experiments/study_3_pilot/station_to_eeg_amp_mapping.csv"

    df = pd.read_csv(csv_path, index_col="experiment_id", dtype=str)

    eeg_devices = []
    for group_session_id, series in df.iterrows():
        group_session_id = str(group_session_id)

        device_id = float(series["lion_actiCHamp"])
        lion_eeg_device = EEGDevice(
            group_session_id=group_session_id,
            station_id="lion",
            device_id=None if math.isnan(device_id) else str(int(device_id))
        )

        device_id = float(series["tiger_actiCHamp"])
        tiger_eeg_device = EEGDevice(
            group_session_id=group_session_id,
            station_id="tiger",
            device_id=None if math.isnan(device_id) else str(int(device_id))
        )

        device_id = float(series["leopard_actiCHamp"])
        leopard_eeg_device = EEGDevice(
            group_session_id=group_session_id,
            station_id="leopard",
            device_id=None if math.isnan(device_id) else str(int(device_id))
        )

        eeg_devices.extend([lion_eeg_device, tiger_eeg_device, leopard_eeg_device])

    db_session = next(get_db())
    db_session.add_all(eeg_devices)
    db_session.commit()
    db_session.close()


def process_base_tables():
    populate_base_tables()
    process_data_validity_workbook()
    process_station_to_eeg_amp_mapping_workbook()
