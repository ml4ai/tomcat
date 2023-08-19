from .tasks import affective_individual


def read_task_db(db_path: str, experiment: str) -> list[dict[str, any]]:
    # db_connection = sqlite3.connect(db_path)
    # task_data = {}
    #
    # # Rest state
    # query = f"""
    #         SELECT *
    #         FROM rest_state_task
    #         WHERE group_session = ?;
    #         """
    # rest_state_df = pd.read_sql_query(query, db_connection, params=[experiment])
    # if len(rest_state_df) > 0:
    #     task_data["rest_state"] = rest_state_df
    #
    # # Finger tapping
    # query = f"""
    #         SELECT *
    #         FROM fingertapping_task_observation
    #         WHERE group_session = ?;
    #         """
    # finger_tapping_df = pd.read_sql_query(query, db_connection, params=[experiment])
    # if len(finger_tapping_df) > 0:
    #     task_data["finger_tapping"] = finger_tapping_df
    #
    # # Affective task individual
    # query = f"""
    #         SELECT *
    #         FROM affective_task_event
    #         WHERE group_session = ? AND task_type = 'individual';
    #         """
    # finger_tapping_df = pd.read_sql_query(query, db_connection, params=[experiment])
    # if len(finger_tapping_df) > 0:
    #     task_data["finger_tapping"] = finger_tapping_df
    #
    # return task_data

    task_data = []

    affective_individual_data = affective_individual(db_path, experiment)
    task_data.append({
        "task_name": "affective_individual",
        "task_data": affective_individual_data
    })

    return task_data
