import glob
import os
from multiprocessing import Pool, Manager

import pandas as pd
from tqdm import tqdm

import logging


def _log_message(message: str, message_queue=None):
    if message_queue is not None:
        message_queue.put(message)
    else:
        logging.warning(message)


def read_raw_csv(csv_path: str, message_queue=None) -> dict[str, any]:
    dtypes = {
        'group_session': str,
        'task': str,
        'station': str,
        'timestamp_iso8601': str,
        'timestamp_unix': float,
        'participant': int,
    }

    exp_df = pd.read_csv(csv_path, dtype=dtypes).drop(columns=["id"])
    lion_df = exp_df[exp_df['station'] == 'lion']
    tiger_df = exp_df[exp_df['station'] == 'tiger']
    leopard_df = exp_df[exp_df['station'] == 'leopard']

    assert lion_df["timestamp_unix"].is_monotonic_increasing
    assert tiger_df["timestamp_unix"].is_monotonic_increasing
    assert leopard_df["timestamp_unix"].is_monotonic_increasing

    exp_name = os.path.splitext(os.path.basename(csv_path))[0]

    if lion_df.empty:
        _log_message(f'{exp_name} No lion data found.', message_queue)
    if tiger_df.empty:
        _log_message(f'{exp_name} No tiger data found.', message_queue)
    if leopard_df.empty:
        _log_message(f'{exp_name} No leopard data found.', message_queue)

    return {
        'experiment_name': exp_name,
        'lion': lion_df,
        'tiger': tiger_df,
        'leopard': leopard_df
    }


def _multiprocess_read_raw_csv(process_arg):
    return read_raw_csv(*process_arg)


def read_raw_csv_all(dir_path: str,
                     num_processes: int = 1,
                     whitelist_experiments: list[str] | None = None) -> list[dict[str, any]]:
    csv_paths = sorted(glob.glob(f'{dir_path}/*.csv'))

    if whitelist_experiments is not None:
        csv_paths = [path for path in csv_paths if os.path.splitext(os.path.basename(path))[0] in whitelist_experiments]

    if len(csv_paths) == 0:
        raise ValueError(f'No CSV files found')

    manager = Manager()
    message_queue = manager.Queue()

    process_args = [(csv_path, message_queue) for csv_path in csv_paths]

    with Pool(processes=num_processes) as pool:
        raw_list = list(tqdm(pool.imap(_multiprocess_read_raw_csv, process_args), total=len(process_args)))

    while not message_queue.empty():
        logging.warning(message_queue.get())

    return raw_list
