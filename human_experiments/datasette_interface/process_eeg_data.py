#!/usr/bin/env python

import os
import sys
import sqlite3

import pandas as pd
import numpy as np
from common import extract_signal_xdf
from utils import (
    cd,
    should_ignore_directory,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
    is_directory_with_white_noise_eeg_channels
)
import pyxdf
import logging
from logging import info, error
from config import DB_PATH, USER
from tqdm import tqdm

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/build_eeg_table.log", mode="w"
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)


def recreate_eeg_table(channel_names: list[str]):
    info("Processing directories...")

    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")
        info("Dropping eeg table")
        db_connection.execute("DROP TABLE IF EXISTS eeg_raw")

        # Generate the SQL code for the EEG channel columns
        channel_columns_sql = ", ".join(f"{name} REAL" for name in channel_names)

        # Now you can use `EEG_channel_columns_sql` in your SQL statement
        create_table = f"CREATE TABLE eeg_raw (" \
                       "group_session TEXT NOT NULL," \
                       "task TEXT," \
                       "station TEXT NOT NULL," \
                       "participant TEXT NOT NULL," \
                       "timestamp_unix TEXT NOT NULL," \
                       "timestamp_iso8601 TEXT NOT NULL," \
                       f"{channel_columns_sql}," \
                       "FOREIGN KEY(group_session) REFERENCES group_session(id)," \
                       "FOREIGN KEY(task) REFERENCES task(id)," \
                       "FOREIGN KEY(station) REFERENCES station(id)," \
                       "FOREIGN KEY(participant) REFERENCES participant(id)" \
                       ");"

        db_connection.execute(create_table)


def insert_raw_unlabeled_data(channel_names: list[str],
                              white_noise_channels: list[str] | None = None):
    exp_info = pd.read_csv('/space/eduong/exp_info_v2/exp_info.csv', dtype=str)

    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        db_connection = sqlite3.connect(DB_PATH)
        with db_connection:
            db_connection.execute("PRAGMA foreign_keys = 1;")

            pbar = tqdm(sorted(directories_to_process), unit="directories")
            for session in pbar:
                pbar.set_description(f'Processing {session}')

                if not is_directory_with_unified_xdf_files(session):
                    process_directory_v1(session,
                                         channel_names,
                                         white_noise_channels)
                else:
                    process_directory_v2(session,
                                         exp_info,
                                         channel_names,
                                         white_noise_channels)


def insert_data_into_table(stream: dict,
                           session: str,
                           station: str,
                           channel_names: list[str],
                           white_noise_channels: list[str] | None = None):
    # We insert a participant ID of -1 since we don't actually know for sure
    # who the participant is - we will need to consult the data validity table
    # to learn the ID, since the originally scheduled participant might be
    # replaced by an experimenter partway through the group session.
    task = np.nan
    participant_id = -1

    if is_directory_with_white_noise_eeg_channels(session):
        remove_channels = white_noise_channels
    else:
        remove_channels = None

    signal_df = extract_signal_xdf(stream,
                                   remove_channels=remove_channels,
                                   desired_channels=channel_names,
                                   unit_conversion=1e-6)

    # Insert experiment data into df
    signal_df.insert(0, 'group_session', session)
    signal_df.insert(1, 'task', task)
    signal_df.insert(2, 'station', station)
    signal_df.insert(3, 'participant', participant_id)
    timestamp_iso8601 = np.array(
        map(convert_unix_timestamp_to_iso8601,
            signal_df["timestamp_unix"])
    )
    signal_df.insert(5, 'timestamp_iso8601', timestamp_iso8601)

    # Insert signals into database
    db_connection = sqlite3.connect(DB_PATH)
    signal_df.to_sql('eeg_raw', db_connection, if_exists='append', index=False)


def process_directory_v1(session: str,
                         channel_names,
                         white_noise_channels):
    info(f"Processing directory {session}")
    with cd(f"{session}"):
        for station in ("lion", "tiger", "leopard"):
            xdf_file = f"{station}/eeg_fnirs_pupil/{station}_eeg_fnirs_pupil.xdf"
            try:
                streams, header = pyxdf.load_xdf(xdf_file, select_streams=[{"type": "EEG"}])
            except ValueError as e:
                error(f"[MISSING DATA]: No EEG stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            stream = streams[0]
            insert_data_into_table(stream,
                                   session,
                                   station,
                                   channel_names,
                                   white_noise_channels)


def _get_station_from_actichamp(exp_info: pd.DataFrame,
                                exp_id: str,
                                actichamp_id: str) -> str | None:
    # Filtering rows that match with experiment_id
    exp_row = exp_info[exp_info['experiment_id'] == exp_id]

    # For the filtered row, check each column (station)
    for col in exp_row.columns:
        # If actiCHamp value matches, return station name (remove "_actiCHamp" part)
        if exp_row[col].values[0] == actichamp_id:
            return col.replace('_actiCHamp', '')

    # If no match found, return None
    return None


def process_directory_v2(session: str,
                         exp_info: pd.DataFrame,
                         channel_names: list[str],
                         white_noise_channels: list[str]):
    """Process directory assuming unified XDF files."""
    info(f"Processing directory {session}")
    with cd(f"{session}/lsl"):
        for xdf_file in ("block_1.xdf", "block_2.xdf"):
            try:
                streams, header = pyxdf.load_xdf(xdf_file, select_streams=[{"type": "EEG"}])
            except ValueError as e:
                error(f"[MISSING DATA]: No EEG stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            for stream in streams:
                actichamp_id = stream["info"]["name"][0].split('-')[1]
                station = _get_station_from_actichamp(exp_info, session, actichamp_id)
                assert station is not None, f"Could not find station for {actichamp_id}!"
                insert_data_into_table(stream,
                                       session,
                                       station,
                                       channel_names,
                                       white_noise_channels)


def create_indices():
    """Create indices for efficient querying"""
    info("Creating index for eeg_raw table.")
    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_group_session_station
            ON eeg_raw (group_session, station);
            """
        )

        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_timestamp_unix
            ON eeg_raw (timestamp_unix);
            """
        )

        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_participant
            ON eeg_raw (participant);
            """
        )

        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_group_session
            ON eeg_raw (group_session);
            """
        )

        db_connection.execute(
            """
            CREATE INDEX IF NOT EXISTS idx_task
            ON eeg_raw (task);
            """
        )


def label_rest_state_task_data(group_session_query: str,
                               station_query: str,
                               is_valid_label: bool,
                               participant_id_label: str,
                               modality_label: str,
                               db_connection: sqlite3.Connection):
    with db_connection:
        start_timestamp, stop_timestamp = db_connection.execute(
            f"""
            SELECT start_timestamp_unix, stop_timestamp_unix from rest_state_task
            WHERE group_session='{group_session_query}';
            """
        ).fetchall()[0]

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE eeg_raw
            SET
                task='rest_state',
                participant='{participant_id_label}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix < '{stop_timestamp}'
                and station='{station_query}'
            """
        )


def label_affective_task_individual_data(group_session_query: str,
                                         station_query: str,
                                         is_valid: bool,
                                         participant_id_query: str,
                                         task,
                                         db_connection: sqlite3.Connection):
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        # Get start/stop timestamps for affective task
        try:
            start_timestamp = db_connection.execute(
                f"""
                SELECT timestamp_unix from affective_task_event
                WHERE
                    group_session='{group_session_query}'
                    AND task_type='individual'
                    AND participant='{participant_id_query}'
                    AND event_type='start_affective_task'
                ORDER BY timestamp_unix LIMIT 1
            """
            ).fetchall()[0][0]
        except IndexError as e:
            error(f"Unable to get start timestamp for "
                  f"{group_session_query}, {station_query}, {participant_id_query}, {task}")
            raise IndexError(e)

        try:
            stop_timestamp = db_connection.execute(
                f"""
                SELECT timestamp_unix from affective_task_event
                WHERE
                    group_session='{group_session_query}'
                    AND task_type='individual'
                    AND participant='{participant_id_query}'
                    AND event_type='final_submission'
                ORDER BY timestamp_unix DESC LIMIT 1
            """
            ).fetchall()[0][0]
        except IndexError as e:
            error(f"Unable to get stop timestamp for "
                  f"{group_session_query}, {station_query}, {participant_id_query}, {task}")
            raise IndexError(e)

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE eeg_raw
            SET
                task='affective_individual',
                participant='{participant_id_query}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix <= '{stop_timestamp}'
                AND station='{station_query}'
            """
        )


def label_affective_task_team_data(group_session_query: str,
                                   station_query: str,
                                   is_valid_label: bool,
                                   participant_id_label: str,
                                   task: str,
                                   db_connection: sqlite3.Connection):
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        # Get start/stop timestamps for affective task
        start_timestamp = db_connection.execute(
            f"""
            SELECT timestamp_unix from affective_task_event
            WHERE
                group_session='{group_session_query}'
                AND task_type='team'
                AND event_type='start_affective_task'
            ORDER BY timestamp_unix LIMIT 1
            """
        ).fetchall()[0][0]

        stop_timestamp = db_connection.execute(
            f"""
            SELECT timestamp_unix from affective_task_event
            WHERE
                group_session='{group_session_query}'
                AND task_type='team'
                AND event_type='final_submission'
            ORDER BY timestamp_unix DESC LIMIT 1
            """
        ).fetchall()[0][0]

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE eeg_raw
            SET
                task='affective_team',
                participant='{participant_id_label}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix <= '{stop_timestamp}'
                AND station='{station_query}'
            """
        )


def update_labels(group_session_query: str,
                  station_query: str,
                  is_valid_label: bool,
                  participant_id_query: str,
                  task: str,
                  db_connection: sqlite3.Connection,
                  table_name: str):
    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        # Get start/stop timestamps for affective task
        try:
            start_timestamp = db_connection.execute(
                f"""
                SELECT timestamp_unix from {table_name}
                WHERE
                    group_session='{group_session_query}'
                ORDER BY timestamp_unix LIMIT 1
                """
            ).fetchall()[0][0]
        except IndexError:
            error(f"""IndexError! Cannot update labels for {group_session_query},
                    {station_query}, {participant_id_query}, {task}, {table_name}.""")

        stop_timestamp = db_connection.execute(
            f"""
            SELECT timestamp_unix from {table_name}
            WHERE
                group_session='{group_session_query}'
            ORDER BY timestamp_unix DESC LIMIT 1
            """
        ).fetchall()[0][0]

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE eeg_raw
            SET
                task='{task}',
                participant='{participant_id_query}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix <= '{stop_timestamp}'
                AND station='{station_query}'
            """
        )


def label_minecraft_data(group_session_query: str,
                         station_query: str,
                         is_valid_label: bool,
                         participant_id_label: str,
                         task: str,
                         db_connection: sqlite3.Connection):
    if task == "saturn_a":
        mission = "Saturn_A"
    elif task == "saturn_b":
        mission = "Saturn_B"
    elif task == "hands_on_training":
        mission = "Hands-on Training"
    else:
        raise ValueError(f"Bad task: {task}!")

    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        # Get start/stop timestamps for affective task
        start_timestamp = db_connection.execute(
            f"""
            SELECT start_timestamp_unix from mission
            WHERE
                group_session='{group_session_query}'
                and name = '{mission}'
            """
        ).fetchall()[0][0]

        stop_timestamp = db_connection.execute(
            f"""
            SELECT stop_timestamp_unix from mission
            WHERE
                group_session='{group_session_query}'
                and name = '{mission}'
            """
        ).fetchall()[0][0]

        # Update participant ID and label task.
        db_connection.execute(
            f"""
            UPDATE eeg_raw
            SET
                task='{task}',
                participant='{participant_id_label}'
            WHERE
                timestamp_unix >= '{start_timestamp}'
                AND timestamp_unix <= '{stop_timestamp}'
                AND station='{station_query}'
            """
        )


def label_data():
    info("Labeling data")
    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        validity_rows = db_connection.execute(
            f"""
            SELECT * from data_validity
            WHERE modality='eeg';
            """
        ).fetchall()

    for row in tqdm(validity_rows):
        group_session, participant_id, station, task, modality, is_valid = row
        if modality != "eeg":
            continue

        if task == "rest_state":
            if group_session == "exp_2022_12_05_12":
                error(
                    f"""
                    [MISSING DATA] There is no rest state data for
                    {group_session}, due to technical issues. See Rick's email
                    from 2023-07-11 for details."""
                )
            else:
                label_rest_state_task_data(
                    group_session, station, is_valid, participant_id, modality, db_connection
                )

        elif "affective" in task:
            if task == "affective_individual":
                label_affective_task_individual_data(
                    group_session, station, is_valid, participant_id, task, db_connection
                )
            elif task == "affective_team":
                label_affective_task_team_data(
                    group_session, station, is_valid, participant_id, task, db_connection
                )
            else:
                raise ValueError(f"Bad task: {task}!")

        elif task == "finger_tapping":
            update_labels(
                group_session, station, is_valid, participant_id, task,
                db_connection, "fingertapping_task_observation"
            )

        elif task == "ping_pong_competitive":
            update_labels(
                group_session, station, is_valid, participant_id, task,
                db_connection, "ping_pong_competitive_task_observation"
            )

        elif task == "ping_pong_cooperative":
            update_labels(
                group_session, station, is_valid, participant_id, task,
                db_connection, "ping_pong_cooperative_task_observation"
            )
        else:
            label_minecraft_data(
                group_session, station, is_valid, participant_id, task,
                db_connection
            )


def remove_invalid_data():
    db_connection = sqlite3.connect(DB_PATH)
    with db_connection:
        validity_rows = db_connection.execute(
            f"""
            SELECT * from data_validity
            WHERE modality='eeg';
            """
        ).fetchall()

    eeg_rows = [row for row in validity_rows if row[-2] == "eeg"]

    for row in tqdm(eeg_rows):
        group_session, participant_id, station, task, modality, is_valid = row

        if is_valid == 0:
            info(
                f"Data for task {task} for {group_session} for modality"
                f" {modality} for station {station}/participant {participant_id}"
                " is not valid. We will delete this data from the table."
            )
            db_connection.execute(
                f"""
                DELETE FROM eeg_raw
                WHERE
                    group_session='{group_session}'
                    AND station='{station}'
                    AND task = '{task}'
                """
            )


if __name__ == "__main__":
    info("Starting building EEG table.")

    EEG_channel_names = [
        "AFF1h",
        "AFF5h",
        "F7",
        "FC5",
        "FC1",
        "C3",
        "T7",
        "TP9",
        "CP5",
        "CP1",
        "Pz",
        "P3",
        "P7",
        "PO9",
        "O1",
        "Oz",
        "O2",
        "PO10",
        "P8",
        "P4",
        "TP10",
        "CP6",
        "CP2",
        "Cz",
        "C4",
        "T8",
        "FC6",
        "FC2",
        "FCz",
        "F8",
        "AFF6h",
        "AFF2h",
        "AUX_GSR",
        "AUX_EKG"
    ]

    white_noise_EEG_channels = [
        'AFF5h',
        'FC1',
        'CP5',
        'CP1',
        'PO9',
        'Oz',
        'PO10',
        'CP6',
        'CP2',
        'FC2',
        'AFF6h'
    ]

    recreate_eeg_table(EEG_channel_names)
    create_indices()
    insert_raw_unlabeled_data(EEG_channel_names, white_noise_EEG_channels)
    label_data()
    remove_invalid_data()
    info("Finished building EEG table.")
