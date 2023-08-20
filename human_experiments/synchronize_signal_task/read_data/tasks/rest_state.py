import sqlite3

import pandas as pd


def rest_state(db_path: str, experiment: str) -> pd.DataFrame | None:
    db = sqlite3.connect(db_path)

    query = f"""
            SELECT * 
            FROM rest_state_task
            WHERE group_session = ?;
            """
    rest_state_df = pd.read_sql_query(query, db, params=[experiment])

    if rest_state_df.empty:
        return None

    rest_state_df = rest_state_df.drop(columns=['group_session',
                                                'start_timestamp_iso8601',
                                                'stop_timestamp_iso8601'])

    rest_state_df = rest_state_df.reset_index(drop=True)

    # Creating a new DataFrame with two rows based on the existing DataFrame
    new_rest_state_df = pd.DataFrame({
        'timestamp_unix': [float(rest_state_df.loc[0, 'start_timestamp_unix']),
                           float(rest_state_df.loc[0, 'stop_timestamp_unix'])],
        'event_type': ['start_task', 'end_task']
    })

    return new_rest_state_df
