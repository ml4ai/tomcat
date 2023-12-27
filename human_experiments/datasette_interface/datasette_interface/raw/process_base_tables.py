#!/usr/bin/env python

import logging
import math
import sys
from logging import info

import pandas as pd
from sqlalchemy import select

from datasette_interface.common.config import LOG_DIR, settings
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.base.data_validity import DataValidity
from datasette_interface.database.entity.base.eeg_device import EEGDevice
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.modality import Modality
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.station import Station
from datasette_interface.database.entity.base.task import Task

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
    db = next(get_db())
    saved_tasks = set(db.scalars(select(Task.id)).all())
    tasks = [Task(id=task) for task in TASKS if task not in saved_tasks]
    if len(tasks) > 0:
        db.add_all(tasks)
        db.commit()
    db.close()


def populate_station_table():
    db = next(get_db())
    saved_stations = set(db.scalars(select(Station.id)).all())
    stations = [
        Station(id=station)
        for station in STATIONS + ["cheetah"]
        if station not in saved_stations
    ]
    if len(stations) > 0:
        db.add_all(stations)
        db.commit()
    db.close()


def populate_modality_table():
    db = next(get_db())
    saved_modalities = set(db.scalars(select(Modality.id)).all())
    modalities = [
        Modality(id=modality) for modality in MODALITIES not in saved_modalities
    ]
    if len(modalities) > 0:
        db.add_all(modalities)
        db.commit()
    db.close()


def populate_participant_table():
    db = next(get_db())
    saved_participants = set(
        db.scalars(select(Participant.id).where(Participant.id < 0)).all()
    )
    # ID of -1 to represent 'unknown participant'
    # ID of -2 to represent the team (for affective task event data)
    # ID of - 3 to represent an unknown experimenter(for the ping pong task data)
    participants = [p for p in [-1, -2, -3] if p not in saved_participants]
    if len(participants) > 0:
        db.add_all(participants)
        db.commit()
    db.close()


def populate_base_tables():
    info("Populating base tables.")

    populate_task_table()
    populate_station_table()
    populate_modality_table()
    populate_participant_table()


def process_data_validity_workbook():
    info("Processing data validity workbook.")

    # TODO Integrate the 'mask_on' statuses.

    df = pd.read_csv(
        settings.data_validity_workbook_path, index_col="experiment_id", dtype=str
    )

    for group_session_id, advisor, series in df.iterrows():
        group_session_id = str(group_session_id)
        advisor = str(advisor)

        if "canceled" in series["lion_subject_id"]:
            continue

        participants = []

        db = next(get_db())
        for prefix in ["lion", "tiger", "leopard"]:
            participant_id = series[f"{prefix}_subject_id"]
            if not db.query(Participant.id).filter_by(id=participant_id).first():
                # Only add participant if it does not exist in the table. SQLAlchemy does not have
                # a DBMS-agnostic treatment for this (e.g. INSERT IGNORE) so the way to do it is
                # to check if the PK exists in the table before inserting the entry into it.
                participants.append(Participant(id=participant_id))

        # For the group session on 2022-09-30, confederate with ID 99901
        # filled in for participant 00012 during the Saturn A mission,
        # since participant 00012 got motion sickness and had to quit the
        # experiment.
        if group_session_id == "exp_2022_09_30_10":
            participant_id = 99901
            if not db.query(Participant.id).filter_by(id=participant_id).first():
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
                        (task == "ping_pong_competitive_0") and (station == "leopard")
                    ):
                        info(
                            f"""
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
                        """
                        )
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
                    participant_id = series[f"{station}_{task_in_csv}_participant_id"]

                    if participant_id == "mission_not_run":
                        continue

                    # Remove the _0, _1 suffixes from the names of the
                    # ping pong competitive tasks before we write them to
                    # the database.
                    task = task.replace("_0", "").replace("_1", "")

                    is_valid = (
                        series[f"{station}_{modality_in_csv}_data_{task_in_csv}"]
                        == "ok"
                    )
                    data_validity = DataValidity(
                        group_session_id=group_session_id,
                        participant_id=participant_id,
                        station_id=station,
                        task_id=task,
                        modality_id=modality,
                        is_valid=is_valid,
                    )

                    data_validity_entries.append(data_validity)

        db.add(GroupSession(id=group_session_id, advisor=advisor))
        db.add_all(participants)
        # Flush to communicate changes to the database in a pending state such that we don't
        # encounter foreign key errors when inserting data validity entries.
        db.flush()

        db.add_all(data_validity_entries)
        db.commit()
        db.close()


def process_station_to_eeg_amp_mapping_workbook():
    info("Process station to eeg amp mapping workbook.")

    df = pd.read_csv(
        settings.station_to_eeg_workbook_path, index_col="experiment_id", dtype=str
    )

    eeg_devices = []
    for group_session_id, series in df.iterrows():
        group_session_id = str(group_session_id)

        device_id = float(series["lion_actiCHamp"])
        lion_eeg_device = EEGDevice(
            group_session_id=group_session_id,
            station_id="lion",
            device_id=None if math.isnan(device_id) else str(int(device_id)),
        )

        device_id = float(series["tiger_actiCHamp"])
        tiger_eeg_device = EEGDevice(
            group_session_id=group_session_id,
            station_id="tiger",
            device_id=None if math.isnan(device_id) else str(int(device_id)),
        )

        device_id = float(series["leopard_actiCHamp"])
        leopard_eeg_device = EEGDevice(
            group_session_id=group_session_id,
            station_id="leopard",
            device_id=None if math.isnan(device_id) else str(int(device_id)),
        )

        eeg_devices.extend([lion_eeg_device, tiger_eeg_device, leopard_eeg_device])

    db = next(get_db())
    db.add_all(eeg_devices)
    db.commit()
    db.close()


def process_base_tables():
    populate_base_tables()
    process_data_validity_workbook()
    process_station_to_eeg_amp_mapping_workbook()
