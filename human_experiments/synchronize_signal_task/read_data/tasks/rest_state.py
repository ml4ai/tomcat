import sqlite3

import pandas as pd


def rest_state(db_path: str, experiment: str) -> pd.DataFrame:
    db = sqlite3.connect(db_path)

    query = f"""
            SELECT * 
            FROM rest_state_task
            WHERE group_session = ?;
            """
    rest_state_df = pd.read_sql_query(query, db, params=[experiment])

    rest_state_df = rest_state_df.drop(columns=['group_session',
                                                'start_timestamp_iso8601',
                                                'stop_timestamp_iso8601'])

    rest_state_df = rest_state_df.reset_index(drop=True)

    return rest_state_df
