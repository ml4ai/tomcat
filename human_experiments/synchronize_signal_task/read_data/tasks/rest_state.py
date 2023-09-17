import pandas as pd
from sqlalchemy import create_engine

from config import POSTGRESQL_ENGINE


def rest_state(experiment: str) -> pd.DataFrame | None:
    query = f"""
            SELECT * 
            FROM rest_state_task
            WHERE group_session = '{experiment}';
            """
    engine = create_engine(POSTGRESQL_ENGINE)
    rest_state_df = pd.read_sql_query(query, engine)

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

    assert new_rest_state_df['timestamp_unix'].is_monotonic_increasing

    return new_rest_state_df
