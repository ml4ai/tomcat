import logging
from multiprocessing import Pool, Manager

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


def _log_message(message: str, message_queue=None):
    if message_queue is not None:
        message_queue.put(message)
    else:
        logging.warning(message)


def read_task_db(experiment: str, message_queue=None) -> dict[str, any]:
    task_data = []

    rest_state_data = rest_state(experiment)
    if rest_state_data is not None:
        task_data.append({
            "task_name": "rest_state",
            "task_data": rest_state_data
        })
    else:
        message = f"ERROR: {experiment} No rest state data found."
        _log_message(message, message_queue)

    finger_tapping_data = finger_tapping(experiment)
    if finger_tapping_data is not None:
        task_data.append({
            "task_name": "finger_tapping",
            "task_data": finger_tapping_data
        })
    else:
        message = f"ERROR: {experiment} No finger tapping data found."
        _log_message(message, message_queue)

    affective_individual_data = affective_individual(experiment)
    if affective_individual_data is not None:
        task_data.append({
            "task_name": "affective_individual",
            "task_data": affective_individual_data
        })
    else:
        message = f"ERROR: {experiment} No affective individual data found."
        _log_message(message, message_queue)

    affective_team_data = affective_team(experiment)
    if affective_team_data is not None:
        task_data.append({
            "task_name": "affective_team",
            "task_data": affective_team_data
        })
    else:
        message = f"ERROR: {experiment} No affective team data found."
        _log_message(message, message_queue)

    ping_pong_competitive_data = ping_pong_competitive(experiment)
    if ping_pong_competitive_data is not None:
        task_data.append({
            "task_name": "ping_pong_competitive",
            "task_data": ping_pong_competitive_data
        })
    else:
        message = f"ERROR: {experiment} No ping pong competitive data found."
        _log_message(message, message_queue)

    ping_pong_cooperative_data = ping_pong_cooperative(experiment)
    if ping_pong_cooperative_data is not None:
        task_data.append({
            "task_name": "ping_pong_cooperative",
            "task_data": ping_pong_cooperative_data
        })
    else:
        message = f"ERROR: {experiment} No ping pong cooperative data found."
        _log_message(message, message_queue)

    minecraft_data = minecraft(experiment)
    if minecraft_data is not None:
        task_data.append({
            "task_name": "minecraft",
            "task_data": minecraft_data
        })

        if 'hands_on_training' not in minecraft_data:
            message = f"ERROR: {experiment} No minecraft hands on training data found."
            _log_message(message, message_queue)
        if 'saturn_a' not in minecraft_data:
            message = f"ERROR: {experiment} No minecraft saturn A data found."
            _log_message(message, message_queue)
        if 'saturn_b' not in minecraft_data:
            message = f"ERROR: {experiment} No minecraft saturn B data found."
            _log_message(message, message_queue)
    else:
        message = f"ERROR: {experiment} No minecraft data found."
        _log_message(message, message_queue)

    task_data_dict = {
        "experiment_name": experiment,
        "task_data": task_data
    }

    return task_data_dict


def _multiprocess_read_task_db(process_arg):
    return read_task_db(*process_arg)


def read_task_db_all(experiments: list[str],
                     num_processes: int = 1) -> list[dict[str, any]]:
    manager = Manager()
    message_queue = manager.Queue()

    process_args = [(experiment, message_queue) for experiment in experiments]

    with Pool(processes=num_processes) as pool:
        results = list(tqdm(pool.imap(_multiprocess_read_task_db, process_args), total=len(process_args)))

    while not message_queue.empty():
        logging.warning(message_queue.get())

    return results
