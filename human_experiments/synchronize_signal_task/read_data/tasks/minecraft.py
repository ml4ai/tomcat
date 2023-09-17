import json

import pandas as pd
from sqlalchemy import create_engine, text

from config import MINECRAFT_MISSION_BLACKLIST, POSTGRESQL_ENGINE


def _get_testbed_messages(experiment: str, mission: str) -> pd.DataFrame | None:
    engine = create_engine(POSTGRESQL_ENGINE)
    conn = engine.connect()

    query = text(
        f"""
        SELECT id
        FROM minecraft_mission
        WHERE group_session = '{experiment}' AND name = '{mission}';
        """
    )

    mission_id = conn.execute(query)

    if mission_id is None:
        return None

    mission_id_result = mission_id.fetchall()
    if mission_id_result is None or len(mission_id_result) == 0:
        return None

    mission_found = False
    for mission_id_considering in mission_id_result:
        if mission_id_considering[0] not in MINECRAFT_MISSION_BLACKLIST:
            mission_id = mission_id_considering[0]
            mission_found = True
            break

    if not mission_found:
        return None

    if len(mission_id) != 36:
        raise ValueError("Incorrect mission ID length")

    query = \
        f"""
        SELECT * 
        FROM minecraft_testbed_message
        WHERE mission = '{mission_id}' AND topic IN ('observations/events/mission', 'observations/events/scoreboard');
        """
    minecraft_df = pd.read_sql_query(query, conn)

    if minecraft_df.empty:
        return None

    minecraft_df = minecraft_df.drop(columns=['mission', 'timestamp_iso8601'])
    minecraft_df['timestamp_unix'] = minecraft_df['timestamp_unix'].astype(float)
    minecraft_df = minecraft_df.sort_values(by='timestamp_unix').reset_index(drop=True)

    # Find the index where the mission starts
    start_index = None
    mission_indices = minecraft_df[minecraft_df['topic'] == 'observations/events/mission'].index
    for idx in mission_indices:
        message = json.loads(minecraft_df.loc[idx, 'message'])
        if message["data"]["mission_state"] == "Start":
            start_index = idx
            break

    # Find the index where the mission ends
    end_index = None
    for idx in mission_indices:
        message = json.loads(minecraft_df.loc[idx, 'message'])
        if message["data"]["mission_state"] == "Stop":
            end_index = idx
            break

    # Keep only the rows between the start and end indices, excluding the start and end rows themselves
    if start_index is not None and end_index is not None:
        minecraft_df = minecraft_df.loc[start_index + 1: end_index - 1]
    elif start_index is not None:  # If there's no end_index, keep everything after the start
        minecraft_df = minecraft_df.loc[start_index + 1:]
    elif end_index is not None:  # End found, no start
        minecraft_df = minecraft_df.loc[:end_index - 1]

    if minecraft_df.empty:
        return None

    minecraft_df = minecraft_df.sort_values(by='timestamp_unix').reset_index(drop=True)

    minecraft_df['points'] = minecraft_df.apply(
        lambda row: json.loads(row['message'])["data"]["scoreboard"]["TeamScore"],
        axis=1
    )

    minecraft_df = minecraft_df.drop(columns=['message', 'topic'])

    return minecraft_df


def minecraft(experiment: str) -> dict[str, pd.DataFrame]:
    training = _get_testbed_messages(experiment, 'Hands-on Training')
    saturn_a = _get_testbed_messages(experiment, 'Saturn_A')
    saturn_b = _get_testbed_messages(experiment, 'Saturn_B')

    minecraft_data = {}
    if training is not None:
        minecraft_data['hands_on_training'] = training

    if saturn_a is not None:
        minecraft_data['saturn_a'] = saturn_a

    if saturn_b is not None:
        minecraft_data['saturn_b'] = saturn_b

    return minecraft_data
