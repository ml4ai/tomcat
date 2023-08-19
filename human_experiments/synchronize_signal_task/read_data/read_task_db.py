from .tasks import (
    affective_individual,
    affective_team,
    rest_state,
    finger_tapping,
    ping_pong_competitive
)


def read_task_db(db_path: str, experiment: str) -> list[dict[str, any]]:
    task_data = []

    rest_state_data = rest_state(db_path, experiment)
    task_data.append({
        "task_name": "rest_state",
        "task_data": rest_state_data
    })

    finger_tapping_data = finger_tapping(db_path, experiment)
    task_data.append({
        "task_name": "finger_tapping",
        "task_data": finger_tapping_data
    })

    affective_individual_data = affective_individual(db_path, experiment)
    task_data.append({
        "task_name": "affective_individual",
        "task_data": affective_individual_data
    })

    affective_team_data = affective_team(db_path, experiment)
    task_data.append({
        "task_name": "affective_team",
        "task_data": affective_team_data
    })

    ping_pong_competitive_data = ping_pong_competitive(db_path, experiment)
    task_data.append({
        "task_name": "ping_pong_competitive",
        "task_data": ping_pong_competitive_data
    })

    return task_data
