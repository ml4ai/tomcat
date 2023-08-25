import sqlite3

import pandas as pd


def ping_pong_competitive(db_path: str, experiment: str) -> dict[tuple[str, str], pd.DataFrame] | None:
    db = sqlite3.connect(db_path)

    query = f"""
            SELECT * 
            FROM ping_pong_competitive_task_observation
            WHERE group_session = ?;
            """
    ping_pong_competitive_df = pd.read_sql_query(query, db, params=[experiment])

    if ping_pong_competitive_df.empty:
        return None

    ping_pong_competitive_df = ping_pong_competitive_df.drop(columns=['group_session',
                                                                      'player_1_id',
                                                                      'player_2_id',
                                                                      'timestamp_iso8601'])

    # Rearrange the columns so that "timestamp_unix" is first
    cols = ping_pong_competitive_df.columns.tolist()
    cols = ['timestamp_unix'] + [col for col in cols if col != 'timestamp_unix']
    ping_pong_competitive_df = ping_pong_competitive_df[cols]

    # Separate the DataFrame into multiple DataFrames based on the unique values of "player_1_station"
    unique_stations = ping_pong_competitive_df['player_1_station'].unique()
    match_dfs = [ping_pong_competitive_df[ping_pong_competitive_df['player_1_station'] == station]
                 for station in unique_stations]

    ids_matches = {}
    for match_df in match_dfs:
        player_1_station = match_df['player_1_station'].unique()[0]
        player_2_station = match_df['player_2_station'].unique()[0]
        match_df = match_df.copy()
        match_df["timestamp_unix"] = match_df["timestamp_unix"].astype(float)
        match_df = match_df.reset_index(drop=True)
        assert match_df["timestamp_unix"].is_monotonic_increasing
        ids_matches[(player_1_station, player_2_station)] = match_df

    return ids_matches
