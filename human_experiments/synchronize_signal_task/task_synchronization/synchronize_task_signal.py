from multiprocessing import Pool

import pandas as pd
from tqdm import tqdm

from .utils import synchronize_task_event_signal, synchronize_task_status_signal


def _filter_stations(df: pd.DataFrame, station_names: list[str]) -> pd.DataFrame:
    # Always include the 'timestamp_unix' column
    columns_to_include = ['timestamp_unix']

    # Check for other columns that include one of the station names
    for column_name in df.columns:
        if any(station in column_name for station in station_names):
            columns_to_include.append(column_name)

    # Return a DataFrame with only the selected columns
    return df[columns_to_include]


def synchronize_task_signal(experiment: dict[str, any]) -> dict[str, any]:
    signal_df = experiment["signals"]
    tasks = experiment["tasks"]

    results = {
        "experiment_name": experiment["experiment_name"]
    }

    for task in tasks:
        task_name = task["task_name"]
        task_data = task["task_data"]

        match task_name:
            case "rest_state" | "affective_team":
                synchronized_df = synchronize_task_event_signal(signal_df, task_data)
                results[task_name] = synchronized_df
            case "finger_tapping" | "ping_pong_cooperative":
                synchronized_df = synchronize_task_status_signal(signal_df, task_data)
                results[task_name] = synchronized_df
            case "affective_individual":
                for station, task_df in task_data.items():
                    station_signal_df = _filter_stations(signal_df, [station])
                    synchronized_df = synchronize_task_event_signal(station_signal_df, task_df)
                    results[f"{task_name}_{station}"] = synchronized_df
            case "ping_pong_competitive":
                for stations, task_df in task_data.items():
                    player_1_station, player_2_station = stations
                    stations_signal_df = _filter_stations(signal_df, [player_1_station, player_2_station])
                    synchronized_df = synchronize_task_status_signal(stations_signal_df, task_df)
                    results[f"{task_name}_{player_1_station}_{player_2_station}"] = synchronized_df
            case "minecraft":
                for mission_name, mission_df in task_data.items():
                    synchronized_df = synchronize_task_status_signal(signal_df, mission_df)
                    results[f"{task_name}_{mission_name}"] = synchronized_df
            case _:
                raise ValueError(f"Unknown task name: {task_name}")

    return results


def synchronize_task_signal_all(experiments: list[dict[str, any]],
                                num_processes: int = 1) -> list[dict[str, any]]:
    with Pool(processes=num_processes) as pool:
        synchronized_list = list(tqdm(pool.imap(synchronize_task_signal, experiments), total=len(experiments)))

    return synchronized_list
