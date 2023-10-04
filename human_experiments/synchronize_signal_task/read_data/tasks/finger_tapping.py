import pandas as pd


def finger_tapping(experiment: str, engine) -> pd.DataFrame | None:
    query = f"""
            SELECT * 
            FROM finger_tapping_task_observation
            WHERE group_session = '{experiment}';
            """
    finger_tapping_df = pd.read_sql_query(query, engine)

    if finger_tapping_df.empty:
        return None

    finger_tapping_df = finger_tapping_df.drop(columns=['group_session',
                                                        'timestamp_iso8601'])

    finger_tapping_df["timestamp_unix"] = finger_tapping_df["timestamp_unix"].astype(float)
    finger_tapping_df = finger_tapping_df.sort_values("timestamp_unix", ascending=True)
    finger_tapping_df = finger_tapping_df.reset_index(drop=True)

    assert finger_tapping_df['timestamp_unix'].is_monotonic_increasing

    return finger_tapping_df
