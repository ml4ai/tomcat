from multiprocessing import Pool

import pandas as pd
from tqdm import tqdm

from sqlalchemy import create_engine
from config import POSTGRESQL_ENGINE

from .utils import (
    group_signal_for_task_event,
    group_signal_for_task_status,
    synchronize_task_status_signal,
    synchronize_affective_team_task_event,
    synchronize_task_event_signal,
    remove_invalid_columns
)


def _remove_invalid_signals(signal_df: pd.DataFrame,
                            signal_types: list[str],
                            experiment_name: str,
                            task: str,
                            engine) -> pd.DataFrame:
    resulting_df = signal_df
    for signal_type in signal_types:
        resulting_df = remove_invalid_columns(
            resulting_df, experiment_name, task, signal_type, engine
        )

    return resulting_df


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
    signal_types = experiment["signal_types"]
    tasks = experiment["tasks"]
    experiment_name = experiment["experiment_name"]

    results = {
        "experiment_name": experiment_name
    }

    engine = create_engine(POSTGRESQL_ENGINE)

    for task in tasks:
        task_name = task["task_name"]
        task_data = task["task_data"]

        match task_name:
            case "rest_state":
                grouped_signal_df = group_signal_for_task_event(
                    signal_df, task_data, check_event_assignments=False
                )
                grouped_signal_df = _remove_invalid_signals(
                    grouped_signal_df, signal_types, experiment_name, task_name, engine
                )
                synchronized_df = synchronize_task_event_signal(grouped_signal_df, task_data)
                results[task_name] = synchronized_df
            case "affective_team":
                grouped_signal_df = group_signal_for_task_event(
                    signal_df, task_data, check_event_assignments=False
                )
                grouped_signal_df = _remove_invalid_signals(
                    grouped_signal_df, signal_types, experiment_name, task_name, engine
                )
                # synchronized_df = synchronize_affective_team_task_event(grouped_signal_df, task_data)
                synchronized_df = synchronize_task_event_signal(grouped_signal_df, task_data)
                results[task_name] = synchronized_df
            case "finger_tapping" | "ping_pong_cooperative":
                grouped_signal_df = group_signal_for_task_status(signal_df, task_data)
                grouped_signal_df = _remove_invalid_signals(
                    grouped_signal_df, signal_types, experiment_name, task_name, engine
                )
                synchronized_df = synchronize_task_status_signal(grouped_signal_df, task_data)
                results[task_name] = synchronized_df
            case "affective_individual":
                for station, task_df in task_data.items():
                    station_signal_df = _filter_stations(signal_df, [station])
                    grouped_station_signal_df = group_signal_for_task_event(
                        station_signal_df, task_df, check_event_assignments=False
                    )
                    grouped_station_signal_df = _remove_invalid_signals(
                        grouped_station_signal_df, signal_types, experiment_name, task_name, engine
                    )
                    synchronized_df = synchronize_task_event_signal(grouped_station_signal_df, task_df)
                    results[f"{task_name}_{station}"] = synchronized_df
            case "ping_pong_competitive":
                for stations, task_df in task_data.items():
                    player_1_station, player_2_station = stations
                    stations_signal_df = _filter_stations(signal_df, [player_1_station, player_2_station])
                    grouped_stations_signal_df = group_signal_for_task_status(stations_signal_df, task_df)
                    grouped_stations_signal_df = _remove_invalid_signals(
                        grouped_stations_signal_df, signal_types, experiment_name, task_name, engine
                    )
                    synchronized_df = synchronize_task_status_signal(grouped_stations_signal_df, task_df)
                    results[f"{task_name}_{player_1_station}_{player_2_station}"] = synchronized_df
            case "minecraft":
                for mission_name, mission_df in task_data.items():
                    grouped_signal_df = group_signal_for_task_status(signal_df, mission_df)
                    grouped_signal_df = _remove_invalid_signals(
                        grouped_signal_df, signal_types, experiment_name, mission_name, engine
                    )
                    synchronized_df = synchronize_task_status_signal(grouped_signal_df, mission_df)
                    results[f"{task_name}_{mission_name}"] = synchronized_df
            case _:
                raise ValueError(f"Unknown task name: {task_name}")

    return results


def synchronize_task_signal_all(experiments: list[dict[str, any]],
                                num_processes: int = 1) -> list[dict[str, any]]:
    with Pool(processes=num_processes) as pool:
        synchronized_list = list(tqdm(pool.imap(synchronize_task_signal, experiments), total=len(experiments)))

    return synchronized_list
