from multiprocessing import Pool

from tqdm import tqdm

from .tasks import (
    affective_individual,
    affective_team,
    rest_state,
    finger_tapping,
    ping_pong_competitive,
    ping_pong_cooperative,
    minecraft
)


def read_task_db(db_path: str, experiment: str) -> dict[str, any]:
    task_data = []

    rest_state_data = rest_state(db_path, experiment)
    if rest_state_data is not None:
        task_data.append({
            "task_name": "rest_state",
            "task_data": rest_state_data
        })

    finger_tapping_data = finger_tapping(db_path, experiment)
    if finger_tapping_data is not None:
        task_data.append({
            "task_name": "finger_tapping",
            "task_data": finger_tapping_data
        })

    affective_individual_data = affective_individual(db_path, experiment)
    if affective_individual_data is not None:
        task_data.append({
            "task_name": "affective_individual",
            "task_data": affective_individual_data
        })

    affective_team_data = affective_team(db_path, experiment)
    if affective_team_data is not None:
        task_data.append({
            "task_name": "affective_team",
            "task_data": affective_team_data
        })

    ping_pong_competitive_data = ping_pong_competitive(db_path, experiment)
    if ping_pong_competitive_data is not None:
        task_data.append({
            "task_name": "ping_pong_competitive",
            "task_data": ping_pong_competitive_data
        })

    ping_pong_cooperative_data = ping_pong_cooperative(db_path, experiment)
    if ping_pong_cooperative_data is not None:
        task_data.append({
            "task_name": "ping_pong_cooperative",
            "task_data": ping_pong_cooperative_data
        })

    minecraft_data = minecraft(db_path, experiment)
    if minecraft_data is not None:
        task_data.append({
            "task_name": "minecraft",
            "task_data": minecraft_data
        })

    task_data_dict = {
        "experiment_name": experiment,
        "task_data": task_data
    }

    return task_data_dict


def _multiprocess_task_db(process_arg: tuple[str, str]) -> dict[str, any]:
    return read_task_db(*process_arg)


def read_task_db_all(db_path: str,
                     experiments: list[str],
                     num_processes: int = 1) -> list[dict[str, any]]:
    process_args = [(db_path, experiment) for experiment in experiments]

    with Pool(processes=num_processes) as pool:
        results = list(tqdm(pool.imap(_multiprocess_task_db, process_args), total=len(process_args)))

    return results
