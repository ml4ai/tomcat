#!/usr/bin/env python

import os
from logging import error, info

import pyxdf
from sqlalchemy import Index
from tqdm import tqdm

from datasette_interface.common.config import settings
from datasette_interface.common.utils import (
    cd,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
    should_ignore_directory,
)
from datasette_interface.database.config import engine, get_db
from datasette_interface.database.entity.base.data_validity import DataValidity
from datasette_interface.raw.common.label_data import (
    delete_invalid_signals,
    label_signals,
)


def insert_raw_unlabeled_data(
        signal_modality_class,
        modality_name,
        xdf_signal_type,
        channel_from_xdf_parsing_fn,
        station_from_xdf_v2_parsing_fn,
        slice_series_fn=lambda x: x,
        swap_channels_fn=None,
):
    info("Inserting unlabeled data.")
    with cd(settings.experiment_root_dir):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        db = next(get_db())
        processed_group_sessions = set(
            [
                s[0]
                for s in db.query(signal_modality_class.group_session_id)
            .distinct(signal_modality_class.group_session_id)
            .all()
            ]
        )

        for group_session in tqdm(sorted(directories_to_process), unit="directories"):
            if group_session in processed_group_sessions:
                info(
                    f"Found saved {modality_name} data for {group_session} in the database. "
                    f"Skipping group session."
                )
                continue

            info(f"Processing directory {group_session}")
            if not is_directory_with_unified_xdf_files(group_session):
                signals = process_directory_v1(
                    group_session,
                    signal_modality_class,
                    modality_name,
                    xdf_signal_type,
                    channel_from_xdf_parsing_fn,
                    slice_series_fn,
                    swap_channels_fn,
                )
            else:
                signals = process_directory_v2(
                    group_session,
                    signal_modality_class,
                    modality_name,
                    xdf_signal_type,
                    channel_from_xdf_parsing_fn,
                    station_from_xdf_v2_parsing_fn,
                    slice_series_fn,
                    swap_channels_fn,
                )

            if len(signals) > 0:
                db.add_all(signals)
                db.commit()
        db.close()


def get_signals(
        stream,
        group_session,
        station,
        initial_id,
        channel_from_xdf_parsing_fn,
        slice_series_fn,
        swap_channels_fn=None,
):
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

        values = slice_series_fn(stream["time_series"][i])
        signal = {
            "group_session_id": group_session,
            "id": i + initial_id,
            "task_id": task,
            "station_id": station,
            "participant_id": participant_id,
            "timestamp_unix": timestamp,
            "timestamp_iso8601": iso_timestamp,
        }
        signal.update(
            {key: value for key, value in zip(channels, list(map(str, values)))}
        )
        if swap_channels_fn:
            swap_channels_fn(signal)

        signals.append(signal)

    return signals


def process_directory_v1(
        group_session,
        signal_modality_class,
        modality_name,
        xdf_signal_type,
        channel_from_xdf_parsing_fn,
        slice_series_fn,
        swap_channels_fn,
):
    signals = []
    with cd(f"{group_session}"):
        for station in ("lion", "tiger", "leopard"):
            xdf_file = f"{station}/eeg_fnirs_pupil/{station}_eeg_fnirs_pupil.xdf"
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
            tmp = [
                signal_modality_class(**signal_dict)
                for signal_dict in get_signals(
                    stream,
                    group_session,
                    station,
                    0,
                    channel_from_xdf_parsing_fn,
                    slice_series_fn,
                    swap_channels_fn,
                )
            ]
            signals.extend(tmp)

    return signals


def process_directory_v2(
        group_session,
        signal_modality_class,
        modality_name,
        xdf_signal_type,
        channel_from_xdf_parsing_fn,
        station_from_xdf_v2_parsing_fn,
        slice_series_fn,
        swap_channels_fn,
):
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
                # For v2, the signals are split in two files (block1 and 2). When processing
                # block2 entries, we need to make sure we start the ID taking into account the
                # last ID in the block 1 files for the same station, otherwise we get a duplicate
                # key error during insertion.
                station = station_from_xdf_v2_parsing_fn(group_session, stream)
                next_id = max_ids.get(station, -1) + 1
                tmp = [
                    signal_modality_class(**signal_dict)
                    for signal_dict in get_signals(
                        stream,
                        group_session,
                        station,
                        next_id,
                        channel_from_xdf_parsing_fn,
                        slice_series_fn,
                        swap_channels_fn,
                    )
                ]
                signals.extend(tmp)
                max_ids[station] = tmp[-1].id

    return signals


def create_indices(signal_modality_class, modality_name):
    """Create indices for efficient querying"""
    info(f"Creating database indices for {modality_name} table.")

    suffix = modality_name.lower()

    idx_task = Index(
        f"idx_task_{suffix}",
        signal_modality_class.group_session_id,
        signal_modality_class.station_id,
        signal_modality_class.task_id,
    )
    idx_task.create(bind=engine, checkfirst=True)


def label_data(signal_modality_class, modality_name):
    info("Labeling data")

    db = next(get_db())
    processed_group_sessions = set(
        [
            s[0]
            for s in db.query(signal_modality_class.group_session_id)
        .distinct(signal_modality_class.group_session_id)
        .filter(signal_modality_class.task_id.is_not(None))
        .all()
        ]
    )

    validity_rows = (
        db.query(
            DataValidity.group_session_id,
            DataValidity.participant_id,
            DataValidity.station_id,
            DataValidity.task_id,
        )
        .filter_by(modality_id=modality_name.lower())
        .all()
    )

    last_group_session_labeled = None
    for row in tqdm(validity_rows):
        group_session, participant_id, station, task = row

        if group_session in processed_group_sessions:
            info(
                f"All {modality_name} entries for {group_session} are labeled in the database. "
                f"Skipping group session."
            )
            continue

        if group_session != last_group_session_labeled:
            info(f"Labeling {group_session}")
            last_group_session_labeled = group_session
            if last_group_session_labeled:
                # Commit per group session
                db.commit()

        label_signals(
            signal_modality_class=signal_modality_class,
            group_session=group_session,
            task=task,
            station=station,
            participant_id=participant_id,
            database_session=db,
        )

    db.commit()
    db.close()


def remove_invalid_data(signal_modality_class, modality_name):
    info("Removing invalid data")

    db = next(get_db())
    invalid_rows = (
        db.query(
            DataValidity.group_session_id,
            DataValidity.station_id,
            DataValidity.participant_id,
            DataValidity.task_id,
        )
        .filter_by(modality_id=modality_name.lower(), is_valid=False)
        .all()
    )

    for row in tqdm(invalid_rows):
        group_session, station, participant_id, task = row

        info(
            f"Data for task {task} for {group_session} for {modality_name}"
            f" modality for station {station}/participant {participant_id}"
            " is not valid. We will delete this data from the table."
        )

        delete_invalid_signals(signal_modality_class, group_session, station, task, db)

    db.commit()
    db.close()


def remove_unlabeled_data(signal_modality_class):
    """
    Remove unlabeled data from the database. That is, data without a task label.
    """
    info("Removing unlabeled data")

    db = next(get_db())
    delete_invalid_signals(
        signal_modality_class,
        group_session=signal_modality_class.group_session_id,
        station=signal_modality_class.station_id,
        task=None,  # no task
        database_session=db,
    )
    db.commit()
    db.close()
