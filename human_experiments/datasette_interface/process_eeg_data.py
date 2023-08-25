#!/usr/bin/env python

import os
import sys
import sqlite3

import pandas as pd
import numpy as np
from common import extract_signal_xdf
from utils import (
    should_ignore_directory,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
    is_directory_with_white_noise_eeg_channels,
    is_directory_ignore_eeg_channels
)
import pyxdf
import logging
from logging import info, error
from config import DB_PATH, USER, NUM_PROCESSES
from tqdm import tqdm
from multiprocessing import Pool

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


def prepare_experiments_info(raw_data_path: str,
                             exp_info_path: str,
                             db_path: str,
                             eeg_channel_names: list[str],
                             white_noise_eeg_channels: list[str]) -> list[dict[str, any]]:
    # Ensure all elements in white_noise_eeg_channels are in eeg_channel_names
    assert all(
        channel in eeg_channel_names
        for channel in white_noise_eeg_channels
    ), "All white noise EEG channels must be in the list of EEG channel names."

    directories_to_process = sorted([
        directory
        for directory in os.listdir(raw_data_path)
        if (not should_ignore_directory(directory) and
            not is_directory_ignore_eeg_channels(directory))
    ])

    exp_info = pd.read_csv(exp_info_path, dtype=str)

    exps_info = [
        {
            "experiment_name": directory,
            "experiment_path": os.path.join(raw_data_path, directory),
            "db_path": db_path,
            "exp_info": exp_info,
            "eeg_channel_names": eeg_channel_names
        }
        for directory in directories_to_process
    ]

    for exp_info in exps_info:
        assert os.path.isdir(exp_info["experiment_path"]), \
            f"Experiment path {exp_info['experiment_path']} does not exist."

        assert os.path.isfile(exp_info["db_path"]), \
            f"DB path {exp_info['db_path']} does not exist."

        if is_directory_with_white_noise_eeg_channels(exp_info["experiment_name"]):
            exp_info["white_noise_eeg_channels"] = white_noise_eeg_channels
        else:
            exp_info["white_noise_eeg_channels"] = None

    return exps_info


def insert_data_into_dataframe(stream: dict,
                               session: str,
                               station: str,
                               channel_names: list[str],
                               white_noise_channels: list[str] | None = None) -> pd.DataFrame:
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

    if not signal_df["timestamp_unix"].is_monotonic_increasing:
        signal_df = signal_df.sort_values(by=["timestamp_unix"])

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

    return signal_df


def process_experiment_v1(session: str,
                          session_path: str,
                          channel_names: list[str],
                          white_noise_channels: list[str]) -> pd.DataFrame:
    all_df = []

    for station in ("lion", "tiger", "leopard"):
        xdf_file = os.path.join(session_path, station, "eeg_fnirs_pupil", f"{station}_eeg_fnirs_pupil.xdf")
        try:
            streams, _ = pyxdf.load_xdf(xdf_file, select_streams=[{"type": "EEG"}])
        except ValueError as e:
            error(f"[MISSING DATA]: No EEG stream found in {xdf_file}!")
            print(e)
            continue
        except Exception as e:
            error(f"[MISSING DATA]: {e}")
            continue

        stream = streams[0]
        station_df = insert_data_into_dataframe(stream,
                                                session,
                                                station,
                                                channel_names,
                                                white_noise_channels)

        all_df.append(station_df)

    result_df = pd.concat(all_df).reset_index(drop=True)

    return result_df


def process_experiment_v2(session: str,
                          session_path: str,
                          exp_info: pd.DataFrame,
                          channel_names: list[str],
                          white_noise_channels: list[str]) -> pd.DataFrame:
    station_df_dict = {}

    for xdf_block in ("block_1.xdf", "block_2.xdf"):
        xdf_file = os.path.join(session_path, "lsl", xdf_block)
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
            station_block_df = insert_data_into_dataframe(stream,
                                                          session,
                                                          station,
                                                          channel_names,
                                                          white_noise_channels)

            if station not in station_df_dict:
                station_df_dict[station] = station_block_df
            else:
                station_df_dict[station] = pd.concat(
                    [station_df_dict[station], station_block_df]
                ).reset_index(drop=True)

    all_df = list(station_df_dict.values())
    result_df = pd.concat(all_df).reset_index(drop=True)

    return result_df


def label_rows_df(signal_df: pd.DataFrame,
                  task: str,
                  start_timestamp: float,
                  stop_timestamp: float,
                  station: str,
                  participant_id_label: str):
    condition = (
            (signal_df['timestamp_unix'] >= start_timestamp) &
            (signal_df['timestamp_unix'] < stop_timestamp) &
            (signal_df['station'] == station)
    )

    signal_df.loc[condition, 'task'] = task
    signal_df.loc[condition, 'participant'] = participant_id_label


def label_rest_state_task_df(signal_df: pd.DataFrame,
                             session: str,
                             station: str,
                             participant_id: str,
                             db_connection: sqlite3.Connection):
    start_timestamp, stop_timestamp = db_connection.execute(
        f"""
        SELECT start_timestamp_unix, stop_timestamp_unix from rest_state_task
        WHERE group_session='{session}';
        """
    ).fetchall()[0]

    label_rows_df(signal_df,
                  'rest_state',
                  float(start_timestamp),
                  float(stop_timestamp),
                  station,
                  participant_id)


def label_affective_task_individual_df(signal_df: pd.DataFrame,
                                       session: str,
                                       station: str,
                                       participant_id: str,
                                       db_connection: sqlite3.Connection):
    # Get start/stop timestamps for affective task
    try:
        start_timestamp = db_connection.execute(
            f"""
                SELECT timestamp_unix from affective_task_event
                WHERE
                    group_session='{session}'
                    AND task_type='individual'
                    AND participant='{participant_id}'
                    AND event_type='start_affective_task'
                ORDER BY timestamp_unix LIMIT 1
            """
        ).fetchall()[0][0]
    except IndexError as e:
        error(f"Unable to get start timestamp for "
              f"{session}, {station}, {participant_id}, affective_individual")
        raise IndexError(e)

    try:
        stop_timestamp = db_connection.execute(
            f"""
                SELECT timestamp_unix from affective_task_event
                WHERE
                    group_session='{session}'
                    AND task_type='individual'
                    AND participant='{participant_id}'
                    AND event_type='final_submission'
                ORDER BY timestamp_unix DESC LIMIT 1
            """
        ).fetchall()[0][0]
    except IndexError as e:
        error(f"Unable to get stop timestamp for "
              f"{session}, {station}, {participant_id}, affective_individual")
        raise IndexError(e)

    label_rows_df(signal_df,
                  'affective_individual',
                  float(start_timestamp),
                  float(stop_timestamp),
                  station,
                  participant_id)


def label_affective_task_team_df(signal_df: pd.DataFrame,
                                 session: str,
                                 station: str,
                                 participant_id: str,
                                 db_connection: sqlite3.Connection):
    start_timestamp = db_connection.execute(
        f"""
            SELECT timestamp_unix from affective_task_event
            WHERE
                group_session='{session}'
                AND task_type='team'
                AND event_type='start_affective_task'
            ORDER BY timestamp_unix LIMIT 1
            """
    ).fetchall()[0][0]

    stop_timestamp = db_connection.execute(
        f"""
            SELECT timestamp_unix from affective_task_event
            WHERE
                group_session='{session}'
                AND task_type='team'
                AND event_type='final_submission'
            ORDER BY timestamp_unix DESC LIMIT 1
            """
    ).fetchall()[0][0]

    label_rows_df(signal_df,
                  'affective_team',
                  float(start_timestamp),
                  float(stop_timestamp),
                  station,
                  participant_id)


def update_labels_df(signal_df: pd.DataFrame,
                     session: str,
                     station: str,
                     participant_id: str,
                     db_connection: sqlite3.Connection,
                     table_name: str,
                     task: str):
    # Get start/stop timestamps for affective task
    try:
        start_timestamp = db_connection.execute(
            f"""
                SELECT timestamp_unix from {table_name}
                WHERE
                    group_session='{session}'
                ORDER BY timestamp_unix LIMIT 1
                """
        ).fetchall()[0][0]
    except IndexError as e:
        error(f"""IndexError! Cannot update labels for {session},
                    {station}, {participant_id}, {task}, {table_name}.""")
        raise IndexError(e)

    stop_timestamp = db_connection.execute(
        f"""
            SELECT timestamp_unix from {table_name}
            WHERE
                group_session='{session}'
            ORDER BY timestamp_unix DESC LIMIT 1
            """
    ).fetchall()[0][0]

    label_rows_df(signal_df,
                  task,
                  float(start_timestamp),
                  float(stop_timestamp),
                  station,
                  participant_id)


def label_minecraft_df(signal_df: pd.DataFrame,
                       session: str,
                       station: str,
                       participant_id: str,
                       db_connection: sqlite3.Connection,
                       task: str):
    if task == "saturn_a":
        mission = "Saturn_A"
    elif task == "saturn_b":
        mission = "Saturn_B"
    elif task == "hands_on_training":
        mission = "Hands-on Training"
    else:
        raise ValueError(f"Bad task: {task}!")

    # Get start/stop timestamps for affective task
    start_timestamp = db_connection.execute(
        f"""
        SELECT start_timestamp_unix from mission
        WHERE
            group_session='{session}'
            and name = '{mission}'
        """
    ).fetchall()[0][0]

    stop_timestamp = db_connection.execute(
        f"""
        SELECT stop_timestamp_unix from mission
        WHERE
            group_session='{session}'
            and name = '{mission}'
        """
    ).fetchall()[0][0]

    label_rows_df(signal_df,
                  task,
                  float(start_timestamp),
                  float(stop_timestamp),
                  station,
                  participant_id)


def label_df(signal_df: pd.DataFrame,
             db_connection: sqlite3.Connection,
             session: str):
    validity_rows = db_connection.execute(
        f"""
        SELECT * from data_validity
        WHERE modality='eeg' AND group_session='{session}';
        """
    ).fetchall()

    for row in validity_rows:
        group_session, participant_id, station, task, modality, is_valid = row

        if task == "rest_state":
            if group_session == "exp_2022_12_05_12":
                error(
                    f"""
                    [MISSING DATA] There is no rest state data for
                    {group_session}, due to technical issues. See Rick's email
                    from 2023-07-11 for details."""
                )
            else:
                label_rest_state_task_df(
                    signal_df, group_session, station, participant_id, db_connection
                )
        elif "affective" in task:
            if task == "affective_individual":
                label_affective_task_individual_df(
                    signal_df, group_session, station, participant_id, db_connection
                )
            elif task == "affective_team":
                label_affective_task_team_df(
                    signal_df, group_session, station, participant_id, db_connection
                )
            else:
                raise ValueError(f"Bad task: {task}!")
        elif task == "finger_tapping":
            update_labels_df(
                signal_df, group_session, station, participant_id,
                db_connection, "fingertapping_task_observation", task
            )
        elif task == "ping_pong_competitive":
            update_labels_df(
                signal_df, group_session, station, participant_id,
                db_connection, "ping_pong_competitive_task_observation", task
            )
        elif task == "ping_pong_cooperative":
            update_labels_df(
                signal_df, group_session, station, participant_id,
                db_connection, "ping_pong_cooperative_task_observation", task
            )
        else:
            label_minecraft_df(
                signal_df, group_session, station, participant_id,
                db_connection, task
            )


def remove_invalid_rows_df(signal_df: pd.DataFrame,
                           db_connection: sqlite3.Connection,
                           session: str):
    validity_rows = db_connection.execute(
        f"""
            SELECT * from data_validity
            WHERE modality='eeg' AND group_session='{session}';
            """
    ).fetchall()

    for row in validity_rows:
        group_session, participant_id, station, task, modality, is_valid = row

        if is_valid == 0:
            info(
                f"Data for task {task} for {group_session} for modality"
                f" {modality} for station {station}/participant {participant_id}"
                " is not valid. We will delete this data from the table."
            )

            condition = (
                    (signal_df['group_session'] == group_session) &
                    (signal_df['station'] == station) &
                    (signal_df['task'] == task)
            )

            signal_df = signal_df.drop(index=signal_df[condition].index)


def process_experiment(experiment: dict[str, any]) -> dict[str, any]:
    exp_data = {
        "experiment_name": experiment["experiment_name"]
    }

    if not is_directory_with_unified_xdf_files(experiment["experiment_name"]):
        exp_signal = process_experiment_v1(experiment["experiment_name"],
                                           experiment["experiment_path"],
                                           experiment["eeg_channel_names"],
                                           experiment["white_noise_eeg_channels"])
    else:
        exp_signal = process_experiment_v2(experiment["experiment_name"],
                                           experiment["experiment_path"],
                                           experiment["exp_info"],
                                           experiment["eeg_channel_names"],
                                           experiment["white_noise_eeg_channels"])

    db_connection = sqlite3.connect(DB_PATH)
    label_df(exp_signal, db_connection, experiment["experiment_name"])
    remove_invalid_rows_df(exp_signal, db_connection, experiment["experiment_name"])

    exp_data["signals"] = exp_signal

    return exp_data


def multiprocess_experiments(experiments: list[dict[str, any]]) -> list[dict[str, any]]:
    with Pool(processes=NUM_PROCESSES) as pool:
        results = list(tqdm(pool.imap(process_experiment, experiments), total=len(experiments)))

    results = sorted(results, key=lambda x: x['experiment_name'])

    return results


def write_experiment_results_to_db(exp_data: list[dict[str, any]]):
    db_connection = sqlite3.connect(DB_PATH)

    with db_connection:
        db_connection.execute("PRAGMA foreign_keys = 1;")

        pbar = tqdm(exp_data, unit="directories")
        for exp in pbar:
            session = exp["experiment_name"]
            pbar.set_description(f'Processing {session}')

            if "signals" in exp:
                exp["signals"].to_sql("eeg_raw", db_connection, if_exists="append", index=False)


def write_experiment_results_to_csv(exp_data: list[dict[str, any]],
                                    output_dir: str):
    pbar = tqdm(exp_data, unit="directories")
    for exp in pbar:
        session = exp["experiment_name"]
        pbar.set_description(f'Processing {session}')

        if "signals" in exp:
            exp["signals"].to_csv(f"{output_dir}/{session}.csv", index=False)


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

    # print("Preparing eeg_raw table.")
    # recreate_eeg_table(EEG_channel_names)
    # create_indices()

    print("Read EEG raw data")
    experiments_info = prepare_experiments_info(
        raw_data_path="/tomcat/data/raw/LangLab/experiments/study_3_pilot/group",
        exp_info_path="/space/eduong/exp_info_v2/exp_info.csv",
        db_path=DB_PATH,
        eeg_channel_names=EEG_channel_names,
        white_noise_eeg_channels=white_noise_EEG_channels
    )

    experiments_data = multiprocess_experiments(experiments_info)

    print("Write EEG data.")
    csv_output_path = f"/space/{USER}/eeg_raw/"
    os.makedirs(csv_output_path, exist_ok=True)
    write_experiment_results_to_csv(experiments_data, csv_output_path)

    # print("Write EEG data to DB.")
    # write_experiment_results_to_db(experiments_data)

    info("Finished building EEG table.")
