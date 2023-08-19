import sqlite3

import numpy as np
import pandas as pd

from common import get_station


def ping_pong_cooperative(db_path: str, experiment: str) -> pd.DataFrame:
    db = sqlite3.connect(db_path)

    query = f"""
            SELECT * 
            FROM ping_pong_cooperative_task_observation
            WHERE group_session = ?;
            """
    ping_pong_competitive_df = pd.read_sql_query(query, db, params=[experiment])

    # Map participant id to station
    player_1_id = ping_pong_competitive_df['player_1_id'].unique()[0]
    player_2_id = ping_pong_competitive_df['player_2_id'].unique()[0]
    player_3_id = ping_pong_competitive_df['player_3_id'].unique()[0]

    player_1_station = get_station(db_path, experiment, player_1_id, "ping_pong_cooperative")
    player_2_station = get_station(db_path, experiment, player_2_id, "ping_pong_cooperative")
    player_3_station = get_station(db_path, experiment, player_3_id, "ping_pong_cooperative")

    id_station_map = {
        player_1_id: player_1_station,
        player_2_id: player_2_station,
        player_3_id: player_3_station
    }

    # Create a new column with the mapped stations
    def map_station(participant):
        return id_station_map.get(participant, np.nan)

    ping_pong_competitive_df['player_1_station'] = ping_pong_competitive_df['player_1_id'].apply(map_station)
    ping_pong_competitive_df['player_2_station'] = ping_pong_competitive_df['player_2_id'].apply(map_station)
    ping_pong_competitive_df['player_3_station'] = ping_pong_competitive_df['player_3_id'].apply(map_station)

    ping_pong_competitive_df = ping_pong_competitive_df.drop(columns=['group_session',
                                                                      'player_1_id',
                                                                      'player_2_id',
                                                                      'player_3_id',
                                                                      'timestamp_iso8601'])

    # Move columns to front
    cols_to_move = ['timestamp_unix', 'player_1_station', 'player_2_station', 'player_3_station']
    other_cols = [col for col in ping_pong_competitive_df.columns if col not in cols_to_move]
    new_order = cols_to_move + other_cols
    ping_pong_competitive_df = ping_pong_competitive_df[new_order]

    return ping_pong_competitive_df
