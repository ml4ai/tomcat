import sqlite3

import pandas as pd

from common import get_station


def affective_individual(db_path: str, experiment: str) -> dict[str, pd.DataFrame]:
    db = sqlite3.connect(db_path)

    query = f"""
            SELECT * 
            FROM affective_task_event
            WHERE group_session = ? AND task_type = 'individual';
            """
    affective_individual_df = pd.read_sql_query(query, db, params=[experiment])

    unique_participants = affective_individual_df['participant'].unique()

    id_station_map = {}
    for participant_id in unique_participants:
        station = get_station(db_path, experiment, participant_id, "affective_individual")
        id_station_map[participant_id] = station

    station_task_map = {}
    for participant_id, station in id_station_map.items():
        station_affective_df = affective_individual_df[affective_individual_df['participant'] == participant_id]
        station_affective_df = station_affective_df.drop(columns=['participant',
                                                                  'task_type',
                                                                  'group_session',
                                                                  'timestamp_iso8601'])
        station_task_map[station] = station_affective_df

    return station_task_map
