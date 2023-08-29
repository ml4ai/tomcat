#!/usr/bin/env python

import logging
import sys
from logging import info

import pandas as pd
from sqlalchemy import create_engine
from sqlalchemy.orm import Session

from config import USER
from entity.base import Base
from entity.data_validity import DataValidity
from entity.group_session import GroupSession
from entity.modality import Modality
from entity.participant import Participant
from entity.station import Station
from entity.task import Task

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_base_tables.log",
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


def populate_task_table(engine):
    with Session(engine) as session:
        modalities = [Task(id=task) for task in TASKS]
        session.add_all(modalities)
        session.commit()


def populate_station_table(engine):
    with Session(engine) as session:
        stations = [Station(id=station) for station in STATIONS + ["cheetah"]]
        session.add_all(stations)
        session.commit()


def populate_modality_table(engine):
    with Session(engine) as session:
        modalities = [Modality(id=modality) for modality in MODALITIES]
        session.add_all(modalities)
        session.commit()


def populate_participant_table(engine):
    with Session(engine) as session:
        participants = [
            # ID of -1 to represent 'unknown participant'
            Participant(id=-1),
            # ID of -2 to represent the team (for affective task event data)
            Participant(id=-2),
            # ID of - 3 to represent an unknown experimenter(for the ping pong task data)
            Participant(id=-3)
        ]
        session.add_all(participants)
        session.commit()


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


def populate_base_tables(engine):
    populate_task_table(engine)
    populate_station_table(engine)
    populate_modality_table(engine)
    populate_participant_table(engine)


def process_rick_workbook(engine):
    """Process Rick's CSVs"""

    # TODO Integrate the 'mask_on' statuses.

    csv_path = "/tomcat/data/raw/LangLab/experiments/study_3_pilot/rchamplin_data_validity_table.csv"

    df = pd.read_csv(csv_path, index_col="experiment_id", dtype=str)

    for group_session_id, series in df.iterrows():
        group_session_id = str(group_session_id)

        if "canceled" in series["lion_subject_id"]:
            continue

        participants = []

        with Session(engine) as session:
            for prefix in ["lion", "tiger", "leopard"]:
                participant_id = series[f"{prefix}_subject_id"]
                if not session.query(Participant.id).filter_by(id=participant_id).first():
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
                if not session.query(Participant.id).filter_by(id=participant_id).first():
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

            session.add(GroupSession(id=group_session_id))
            session.add_all(participants)
            session.add_all(data_validity_entries)
            session.commit()


if __name__ == "__main__":
    engine = create_engine("postgresql+psycopg2://paulosoares:tomcat@localhost:5433/tomcat")

    Base.metadata.drop_all(engine, checkfirst=True)
    Base.metadata.create_all(engine)

    populate_base_tables(engine)
    process_rick_workbook(engine)
