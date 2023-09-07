#!/usr/bin/env python

import os
from logging import info, error

import pyxdf
from sqlalchemy import Index
from sqlalchemy.orm import Session
from tqdm import tqdm

from entity.base.data_validity import DataValidity
from label_data import delete_invalid_signals
from label_data import label_signals
from utils import (
    cd,
    should_ignore_directory,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
)


def insert_raw_unlabeled_data(database_engine, override, signal_modality_class, modality_name, xdf_signal_type,
                              channel_from_xdf_parsing_fn, station_from_xdf_v2_parsing_fn):
    info(f"Inserting unlabeled data.")
    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        with Session(database_engine.connect()) as database_session:
            processed_group_sessions = set(
                [s[0] for s in database_session.query(signal_modality_class.group_session_id).distinct(
                    signal_modality_class.group_session_id).all()])

            for group_session in tqdm(
                    sorted(directories_to_process), unit="directories"
            ):
                if not override and group_session in processed_group_sessions:
                    info(
                        f"Found saved {modality_name} data for {group_session} in the database. Skipping group session.")
                    continue

                info(f"Processing directory {group_session}")
                if not is_directory_with_unified_xdf_files(group_session):
                    signals = process_directory_v1(group_session,
                                                   signal_modality_class,
                                                   modality_name,
                                                   xdf_signal_type,
                                                   channel_from_xdf_parsing_fn)
                else:
                    signals = process_directory_v2(group_session,
                                                   signal_modality_class,
                                                   modality_name,
                                                   xdf_signal_type,
                                                   channel_from_xdf_parsing_fn,
                                                   station_from_xdf_v2_parsing_fn)

                if len(signals) > 0:
                    database_session.add_all(signals)
                    database_session.commit()


def get_signals(stream, group_session, station, initial_id, signal_modality_class, channel_from_xdf_parsing_fn):
    # We insert a participant ID of -1 since we don't actually know for sure
    # who the participant is - we will need to consult the data validity table
    # to learn the ID, since the originally scheduled participant might be
    # replaced by an experimenter partway through the group session.
    task = None
    participant_id = -1
    channels = channel_from_xdf_parsing_fn(stream)

    signals = []
    for i, timestamp in enumerate(stream["time_stamps"]):
        try:
            iso_timestamp = convert_unix_timestamp_to_iso8601(timestamp)
        except ValueError as e:
            error(f"[INVALID DATA]: Error in {i + 1}-th timestamp. {e}")
            print(stream)
            continue

        signal = signal_modality_class(group_session_id=group_session,
                                       id=i + initial_id,
                                       task_id=task,
                                       station_id=station,
                                       participant_id=participant_id,
                                       timestamp_unix=timestamp,
                                       timestamp_iso8601=iso_timestamp,
                                       **{key: value for key, value in
                                          zip(channels, list(map(str, stream["time_series"][i])))})
        signals.append(signal)

    return signals


def process_directory_v1(group_session, signal_modality_class, modality_name, xdf_signal_type,
                         channel_from_xdf_parsing_fn):
    signals = []
    with cd(f"{group_session}"):
        for station in ("lion", "tiger", "leopard"):
            xdf_file = (
                f"{station}/eeg_fnirs_pupil/{station}_eeg_fnirs_pupil.xdf"
            )
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"type": xdf_signal_type}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No {modality_name} stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            stream = streams[0]
            signals.extend(
                get_signals(stream, group_session, station, 0, signal_modality_class, channel_from_xdf_parsing_fn))

    return signals


def process_directory_v2(group_session, signal_modality_class, modality_name, xdf_signal_type,
                         channel_from_xdf_parsing_fn, station_from_xdf_v2_parsing_fn):
    """Process directory assuming unified XDF files."""
    signals = []
    with cd(f"{group_session}/lsl"):
        max_ids = {}
        for xdf_file in ("block_1.xdf", "block_2.xdf"):
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"type": xdf_signal_type}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No {modality_name} stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            for stream in streams:
                # For v2, the signals are split in two files (block1 and 2). When processing block2 entries,
                # we need to make sure we start the ID taking into account the last ID in the block 1 files for the same
                # station, otherwise we get a duplicate key error during insertion.
                station = station_from_xdf_v2_parsing_fn(stream)
                next_id = max_ids.get(station, -1) + 1
                tmp = get_signals(stream, group_session, station, next_id, signal_modality_class,
                                  channel_from_xdf_parsing_fn)
                signals.extend(tmp)
                max_ids[station] = tmp[-1].id

    return signals


def create_indices(database_engine, check, signal_modality_class, modality_name):
    """Create indices for efficient querying"""
    info(f"Creating database indices for {modality_name} table.")

    idx_group_session_station = Index('idx_group_session_station', signal_modality_class.group_session_id,
                                      signal_modality_class.station_id)
    idx_group_session_station.create(bind=database_engine, checkfirst=check)

    idx_timestamp_unix = Index('idx_timestamp_unix', signal_modality_class.timestamp_unix)
    idx_timestamp_unix.create(bind=database_engine, checkfirst=check)

    idx_participant = Index('idx_participant', signal_modality_class.participant_id)
    idx_participant.create(bind=database_engine, checkfirst=check)

    idx_group_session = Index('idx_group_session', signal_modality_class.group_session_id)
    idx_group_session.create(bind=database_engine, checkfirst=check)

    idx_task = Index('idx_task', signal_modality_class.group_session_id)
    idx_task.create(bind=database_engine, checkfirst=check)


def label_data(database_engine, override, signal_modality_class, modality_name):
    info("Labeling data")

    with Session(database_engine) as database_session:
        processed_group_sessions = set([s[0] for s in
                                        database_session.query(signal_modality_class.group_session_id).distinct(
                                            signal_modality_class.group_session_id).filter(
                                            signal_modality_class.task_id.is_not(None)).all()])

        validity_rows = database_session.query(DataValidity.group_session_id,
                                               DataValidity.participant_id,
                                               DataValidity.station_id,
                                               DataValidity.task_id).filter_by(modality_id=modality_name.lower()).all()

        last_group_session_labeled = None
        for row in tqdm(validity_rows):
            group_session, participant_id, station, task = row

            if not override and group_session in processed_group_sessions:
                info(
                    f"All {modality_name} entries for {group_session} are labeled in the database. Skipping group session.")
                continue

            if group_session != last_group_session_labeled:
                info(f"Labeling {group_session}")
                last_group_session_labeled = group_session
                if last_group_session_labeled:
                    # Commit per group session
                    database_session.commit()

            label_signals(
                signal_modality_class=signal_modality_class,
                group_session=group_session,
                task=task,
                station=station,
                participant_id=participant_id,
                database_session=database_session
            )

        database_session.commit()


def remove_invalid_data(database_engine, signal_modality_class, modality_name):
    info("Removing invalid data")

    with Session(database_engine) as database_session:
        invalid_rows = database_session.query(DataValidity.group_session_id,
                                              DataValidity.station_id,
                                              DataValidity.participant_id,
                                              DataValidity.task_id).filter_by(modality_id=modality_name.lower(),
                                                                              is_valid=False).all()

        for row in tqdm(invalid_rows):
            group_session, station, participant_id, task = row

            info(
                f"Data for task {task} for {group_session} for {modality_name}"
                f" modality for station {station}/participant {participant_id}"
                " is not valid. We will delete this data from the table."
            )

            delete_invalid_signals(signal_modality_class, group_session, station, task, database_session)

        database_session.commit()
