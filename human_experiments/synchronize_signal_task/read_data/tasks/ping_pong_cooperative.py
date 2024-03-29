import numpy as np
import pandas as pd

from common import get_station


def ping_pong_cooperative(experiment: str, engine) -> pd.DataFrame | None:
    query = f"""
            SELECT * 
            FROM ping_pong_cooperative_task_observation
            WHERE group_session = '{experiment}';
            """
    ping_pong_competitive_df = pd.read_sql_query(query, engine)

    if ping_pong_competitive_df.empty:
        return None

    # Map participant id to station
    player_1_id = ping_pong_competitive_df['player_1_id'].unique()[0]
    player_2_id = ping_pong_competitive_df['player_2_id'].unique()[0]
    player_3_id = ping_pong_competitive_df['player_3_id'].unique()[0]

    player_1_station = get_station(experiment, player_1_id, "ping_pong_cooperative", engine)
    player_2_station = get_station(experiment, player_2_id, "ping_pong_cooperative", engine)
    player_3_station = get_station(experiment, player_3_id, "ping_pong_cooperative", engine)

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

    ping_pong_competitive_df["timestamp_unix"] = ping_pong_competitive_df["timestamp_unix"].astype(float)
    ping_pong_competitive_df = ping_pong_competitive_df.sort_values("timestamp_unix", ascending=True)

    assert ping_pong_competitive_df["timestamp_unix"].is_monotonic_increasing

    return ping_pong_competitive_df
