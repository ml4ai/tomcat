import sqlite3

import pandas as pd


def finger_tapping(db_path: str, experiment: str) -> pd.DataFrame | None:
    db = sqlite3.connect(db_path)

    query = f"""
            SELECT * 
            FROM fingertapping_task_observation
            WHERE group_session = ?;
            """
    finger_tapping_df = pd.read_sql_query(query, db, params=[experiment])

    if finger_tapping_df.empty:
        return None

    finger_tapping_df = finger_tapping_df.drop(columns=['group_session',
                                                        'timestamp_iso8601'])

    finger_tapping_df["timestamp_unix"] = finger_tapping_df["timestamp_unix"].astype(float)
    finger_tapping_df = finger_tapping_df.reset_index(drop=True)

    assert finger_tapping_df['timestamp_unix'].is_monotonic_increasing

    return finger_tapping_df
