#!/usr/bin/env python

import logging
import os
import sys
from logging import info, error

import pyxdf
from sqlalchemy import Index
from sqlalchemy.orm import Session
from tqdm import tqdm

from config import USER
from entity.base.data_validity import DataValidity
from entity.signal.fnirs import FNIRSRaw
from label_data import delete_invalid_signals
from label_data import label_signals
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
            filename=f"/space/{USER}/tomcat/build_fnirs_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def insert_raw_unlabeled_data(database_engine):
    info(f"Inserting unlabeled data.")
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

                if not is_directory_with_unified_xdf_files(group_session):
                    signals = process_directory_v1(group_session)
                else:
                    signals = process_directory_v2(group_session)

                if len(signals) > 0:
                    database_session.add_all(signals)
                    database_session.commit()


def get_signals(stream, group_session, station):
    # We insert a participant ID of -1 since we don't actually know for sure
    # who the participant is - we will need to consult the data validity table
    # to learn the ID, since the originally scheduled participant might be
    # replaced by an experimenter partway through the group session.
    task = None
    participant_id = -1
    channels = stream["info"]["desc"][0]["channels"][0]["channel"][41:]
    print(channels)
    signals = [
        FNIRSRaw(group_session_id=group_session,
                 id=i,
                 task_id=task,
                 station_id=station,
                 participant_id=participant_id,
                 timestamp_unix=timestamp,
                 timestamp_iso8601=convert_unix_timestamp_to_iso8601(timestamp),
                 **{key: value for key, value in zip(channels, list(map(str, stream["time_series"][i][41:])))})
        for i, timestamp in enumerate(stream["time_stamps"])]

    return signals


def process_directory_v1(group_session):
    signals = []
    with cd(f"{group_session}"):
        for station in ("lion", "tiger", "leopard"):
            xdf_file = (
                f"{station}/eeg_fnirs_pupil/{station}_eeg_fnirs_pupil.xdf"
            )
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"type": "NIRS"}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No fNIRS stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            stream = streams[0]
            signals.extend(get_signals(stream, group_session, station))

    return signals


def process_directory_v2(group_session):
    """Process directory assuming unified XDF files."""
    signals = []
    with cd(f"{group_session}/lsl"):
        for xdf_file in ("block_1.xdf", "block_2.xdf"):
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"type": "NIRS"}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No fNIRS stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            for stream in streams:
                station = stream["info"]["name"][0].split("_")[0]
                signals.extend(get_signals(stream, group_session, station))

    return signals


def create_indices(database_engine):
    """Create indices for efficient querying"""
    info("Creating database indices for fnirs_raw table.")

    idx_group_session_station = Index('idx_group_session_station', FNIRSRaw.group_session_id, FNIRSRaw.station_id)
    idx_group_session_station.create(bind=database_engine)

    idx_timestamp_unix = Index('idx_timestamp_unix', FNIRSRaw.timestamp_unix)
    idx_timestamp_unix.create(bind=database_engine)

    idx_participant = Index('idx_participant', FNIRSRaw.participant_id)
    idx_participant.create(bind=database_engine)

    idx_group_session = Index('idx_group_session', FNIRSRaw.group_session_id)
    idx_group_session.create(bind=database_engine)

    idx_task = Index('idx_task', FNIRSRaw.group_session_id)
    idx_task.create(bind=database_engine)


def label_data(database_engine):
    info("Labeling data")

    with Session(database_engine) as database_session:
        validity_rows = database_session.query(DataValidity.group_session_id,
                                               DataValidity.participant_id,
                                               DataValidity.station_id,
                                               DataValidity.task_id).filter_by(modality_id="fnirs").all()

        for row in tqdm(validity_rows):
            group_session, participant_id, station, task = row

            info(f"Labeling {group_session}")

            label_signals(
                signal_modality_class=FNIRSRaw,
                group_session=group_session,
                task=task,
                station=station,
                participant_id=participant_id,
                database_session=database_session
            )

        database_session.commit()


def remove_invalid_data(database_engine):
    info("Removing invalid data")

    with Session(database_engine) as database_session:
        invalid_rows = database_session.query(DataValidity.group_session_id,
                                              DataValidity.station_id,
                                              DataValidity.participant_id,
                                              DataValidity.task_id).filter_by(modality_id="fnirs",
                                                                              is_valid=False).all()

        for row in tqdm(invalid_rows):
            group_session, station, participant_id, task = row

            info(
                f"Data for task {task} for {group_session} for fnirs"
                f" modality for station {station}/participant {participant_id}"
                " is not valid. We will delete this data from the table."
            )

            delete_invalid_signals(FNIRSRaw, group_session, station, task, database_session)

        database_session.commit()


def process_fnirs_raw_data(database_engine):
    info("Processing fNIRS data.")
    insert_raw_unlabeled_data(database_engine)
    create_indices(database_engine)
    label_data(database_engine)
    remove_invalid_data(database_engine)


def recreate_fnirs_raw_tables(database_engine):
    FNIRSRaw.__table__.drop(database_engine, checkfirst=True)
    FNIRSRaw.__table__.create(database_engine, checkfirst=True)