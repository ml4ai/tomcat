import sqlite3

import numpy as np
import pandas as pd

from common import get_station


def affective_individual(db_path: str, experiment: str) -> dict[str, pd.DataFrame] | None:
    db = sqlite3.connect(db_path)

    query = f"""
            SELECT * 
            FROM affective_task_event
            WHERE group_session = ? AND task_type = 'individual';
            """
    affective_individual_df = pd.read_sql_query(query, db, params=[experiment])

    if affective_individual_df.empty:
        return None

    # Map participant id to station
    unique_participants = affective_individual_df['participant'].unique()
    id_station_map = {}
    for participant_id in unique_participants:
        station = get_station(db_path, experiment, participant_id, "affective_individual")
        id_station_map[participant_id] = station

    # Create a new column with the mapped stations
    def map_station(participant):
        return id_station_map.get(participant, np.nan)

    station_task_map = {}
    for participant_id, station in id_station_map.items():
        station_affective_df = affective_individual_df[affective_individual_df['participant'] == participant_id]
        station_affective_df = station_affective_df.copy()
        station_affective_df['station'] = station_affective_df['participant'].apply(map_station)
        station_affective_df = station_affective_df.drop(columns=['participant',
                                                                  'task_type',
                                                                  'group_session',
                                                                  'timestamp_iso8601'])

        # Reindex the DataFrame according to the new column order
        cols = station_affective_df.columns.tolist()
        cols = [cols[0]] + ['station'] + [col for col in cols[1:] if col != 'station']
        station_affective_df = station_affective_df.reindex(columns=cols)

        station_affective_df["timestamp_unix"] = station_affective_df["timestamp_unix"].astype(float)
        station_affective_df = station_affective_df.reset_index(drop=True)

        assert station_affective_df["timestamp_unix"].is_monotonic_increasing

        station_task_map[station] = station_affective_df

    return station_task_map
