import sqlite3

import numpy as np
import pandas as pd

from common import get_station


def affective_team(db_path: str, experiment: str) -> pd.DataFrame | None:
    db = sqlite3.connect(db_path)

    query = f"""
            SELECT * 
            FROM affective_task_event
            WHERE group_session = ? AND task_type = 'team';
            """
    affective_team_df = pd.read_sql_query(query, db, params=[experiment])

    if affective_team_df.empty:
        return None

    # Map participant id to station
    unique_participants = affective_team_df['participant'].unique()
    id_station_map = {}
    for participant_id in unique_participants:
        if participant_id < 0:
            continue
        station = get_station(db_path, experiment, participant_id, "affective_team")
        id_station_map[participant_id] = station

    # Create a new column with the mapped stations
    def map_station(participant):
        return id_station_map.get(participant, np.nan)

    affective_team_df = affective_team_df.copy()
    affective_team_df['station'] = affective_team_df['participant'].apply(map_station)

    affective_team_df = affective_team_df.drop(columns=['participant',
                                                        'task_type',
                                                        'group_session',
                                                        'timestamp_iso8601'])

    # Reindex the DataFrame according to the new column order
    cols = affective_team_df.columns.tolist()
    cols = [cols[0]] + ['station'] + [col for col in cols[1:] if col != 'station']
    affective_team_df = affective_team_df.reindex(columns=cols)

    affective_team_df["timestamp_unix"] = affective_team_df["timestamp_unix"].astype(float)
    affective_team_df = affective_team_df.reset_index(drop=True)

    return affective_team_df
