#!/usr/bin/env python

import logging
import math
import sys
from logging import info

import pandas as pd
from sqlalchemy import select
from sqlalchemy.exc import NoResultFound
from tqdm import tqdm

from datasette_interface.common.config import LOG_DIR, settings
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.base.data_validity import DataValidity
from datasette_interface.database.entity.base.eeg_device import EEGDevice
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.database.entity.base.modality import Modality
from datasette_interface.database.entity.base.participant import Participant
from datasette_interface.database.entity.base.post_game_survey import (
    PostGameSurvey,
)
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
        Modality(id=modality)
        for modality in MODALITIES
        if modality not in saved_modalities
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
    # ID of -3 to represent an unknown experimenter (for the ping pong task data)
    participants = [
        Participant(id=p) for p in [-1, -2, -3] if p not in saved_participants
    ]
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
        settings.data_validity_workbook_path,
        index_col="experiment_id",
        dtype=str,
    )

    db = next(get_db())
    for group_session_id, series in df.iterrows():
        group_session_id = str(group_session_id)

        if "canceled" in series["lion_subject_id"]:
            continue

        if (
            db.scalar(
                select(GroupSession.id).where(GroupSession.id == group_session_id)
            )
            is not None
        ):
            info(
                f"Found group session {group_session_id} in the group_session table. Skipping."
            )
            continue

        participants = []

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
        tasks_new = TASKS + [
            "ping_pong_competitive_0",
            "ping_pong_competitive_1",
        ]
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

        db.add(GroupSession(id=group_session_id, advisor=series["advisor"]))
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
        settings.station_to_eeg_workbook_path,
        index_col="experiment_id",
        dtype=str,
    )

    db = next(get_db())
    eeg_devices = []
    for group_session_id, series in df.iterrows():
        group_session_id = str(group_session_id)

        if (
            db.scalar(
                select(EEGDevice.group_session_id).where(
                    EEGDevice.group_session_id == group_session_id
                )
            )
            is not None
        ):
            info(
                f"Found group session {group_session_id} in the eeg_device table. Skipping."
            )
            continue

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

    db.add_all(eeg_devices)
    db.commit()
    db.close()


def process_demographic_data():
    db = next(get_db())
    data_dictionary_df = pd.read_table(
        settings.self_report_data_dictionary_path, index_col=0
    )

    demographics_fields = [
        "age",
        "sex",
        "hisp",
        "race",
        "income",
        "edu",
        "exp",
        "exp_mc",
        "handedness",
        "trackpad_preference",
        "shl_impairments",
        "shl_impairment_specify",
        "shl_impairment_agediagnosis",
        "shl_impairment_therapy",
        "first_language",
        "languages_spoken",
        "language_age_learned",
        "countries_live_one_year",
        "major_schooling_country",
        "health_concussion",
        "health_seizure",
        "health_trauma",
        "health_other_trauma_specify",
        "health_medications",
        "health_vision",
        "health_vision_specify",
    ]

    df = pd.read_table(
        settings.self_report_data_path,
        usecols=["subject_id"]
        + [k.replace("impairments", "impairements") for k in demographics_fields],
    )

    for i, row in df.iterrows():
        # Check if subject ID is in table
        try:
            participant = db.scalars(
                select(Participant).where(Participant.id == f"{row['subject_id']}")
            ).one()
            for label in row.index:
                field = data_dictionary_df.loc[label]
                field_type = field["Field Type"]
                entry = row.loc[label]
                if pd.isna(entry):
                    entry = None

                if entry is not None:
                    if field_type == "radio":
                        choices = field["Choices, Calculations, OR Slider Labels"]
                        choices = {
                            int(k): v
                            for k, v in [
                                x.strip().split(", ") for x in choices.split("|")
                            ]
                        }
                        row.loc[label] = choices[row.loc[label]]

            # Participant 14 entered their age as 18` instead of 18.
            if row.loc["age"] == "18`":
                row.loc["age"] = 18

            for attr in row.index:
                value = row.loc[attr]
                if pd.isna(value):
                    value = None
                setattr(participant, attr, value)
            db.commit()
        except NoResultFound:
            pass

    db.close()


def process_post_game_survey():
    info("Processing post game survey data...")
    data_dictionary_df = pd.read_csv(
        settings.post_game_survey_data_dictionary_path, index_col=0
    )

    post_game_survey_fields = [
        "agent_calm",
        "agent_anxious",
        "agent_excited",
        "agent_sad",
        "agent_guilty",
        "agent_angry",
        "agent_happy",
        "agent_lonely",
        "agent_proud",
        "agent_friendly",
        "game_calm",
        "game_anxious",
        "game_excited",
        "game_sad",
        "game_guilty",
        "game_angry",
        "game_happy",
        "game_lonely",
        "game_proud",
        "game_friendly",
        "agent_intel",
        "agent_care",
        "agent_honest",
        "agent_expert",
        "agent_concern",
        "agent_trust",
        "agent_comp",
        "agent_insens",
        "agent_honor",
        "agent_bright",
        "agent_understand",
        "agent_phoney",
        "agent_well",
        "agent_smooth",
        "agent_acc",
        "agent_like",
        "agent_enjoy",
        "agent_awk",
        "agent_place",
        "agent_play",
        "agent_perform",
        "team_calm",
        "team_anxious",
        "team_excited",
        "team_sad",
        "team_guilty",
        "team_angry",
        "team_happy",
        "team_lonely",
        "team_proud",
        "team_friendly",
        "team_intel",
        "team_care",
        "team_honest",
        "team_expert",
        "team_concern",
        "team_trust",
        "team_comp",
        "team_insens",
        "team_honor",
        "team_bright",
        "team_understand",
        "team_phoney",
        "team_wrong",
        "team_forget",
        "team_minimize",
        "team_insult",
        "team_irrat",
        "team_crit",
        "team_fault",
        "team_ignore",
        "team_impt",
        "team_weak",
        "team_along",
        "team_smooth",
        "team_accept",
        "team_like",
        "team_enjoy",
        "team_awk",
        "team_place",
        "team_likeme",
        "team_genuine",
        "team_part",
        "team_listen",
        "team_ilike",
        "team_interact",
        "team_itrust",
        "team_fit",
        "team_cohesion",
        "team_work",
        "team_knit",
        "team_like_mem",
        "team_work_well",
        "team_decision",
        "team_express",
        "team_organize",
        "team_accomplish",
        "team_approp",
        "team_alt",
        "team_influ",
        "team_contrib",
        "agent_emot1",
        "agent_emot2",
        "agent_emot3",
        "agent_emot4",
        "agent_emot5",
        "agent_emot6",
        "agent_emot7",
        "agent_emot8",
        "agent_emot9",
        "agent_emot10",
        "know_team_members",
        "know_person_at_cheetah",
        "know_person_at_lion",
        "know_person_at_tiger",
        "know_person_at_leopard",
    ]

    df = pd.read_csv(
        settings.post_game_survey_data_path,
        usecols=["subject_id"]
        + [
            k.replace("agent_intel", "agent_intell")
            .replace("decision", "decisision")
            .replace("team_guilty", "team_guilt")
            for k in post_game_survey_fields
        ]
        + ["postgame_survey_timestamp"],
    )

    db = next(get_db())
    for i, row in tqdm(df.iterrows(), total=len(df)):
        participant_id = None
        # Subject ID 63 did not finish the post-game survey in the first
        # attempt, so we ignore the first attempt.
        if row.postgame_survey_timestamp == "[not completed]" or pd.isna(
            row.postgame_survey_timestamp
        ):
            continue

        try:
            participant_id = int(row["subject_id"])
        except ValueError:
            continue

        try:
            _ = db.scalars(
                select(Participant).where(Participant.id == participant_id)
            ).one()
        except NoResultFound:
            continue

        for label in row.index:
            if label != "postgame_survey_timestamp":
                field = data_dictionary_df.loc[label]
                field_type = field["Field Type"]
                entry = row.loc[label]
                if pd.isna(entry):
                    entry = None

                if entry is not None:
                    if field_type == "radio":
                        choices = field["Choices, Calculations, OR Slider Labels"]
                        choices = {
                            int(k): v
                            for k, v in [
                                x.strip().split(", ") for x in choices.split("|")
                            ]
                        }
                        row.loc[label] = choices[row.loc[label]]

        post_game_survey = PostGameSurvey(participant_id=participant_id)
        for attr in row.index:
            if attr not in ["subject_id", "postgame_survey_timestamp"]:
                value = row.loc[attr]
                if pd.isna(value):
                    value = None
                setattr(post_game_survey, attr, value)
        db.add(post_game_survey)
        db.flush()
        db.commit()
    db.close()


def process_base_tables():
    populate_base_tables()
    process_data_validity_workbook()
    process_station_to_eeg_amp_mapping_workbook()
    process_demographic_data()
    process_post_game_survey()
