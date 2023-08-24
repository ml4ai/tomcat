import glob
import os
from multiprocessing import Pool

import pandas as pd
from tqdm import tqdm


def read_raw_csv(csv_path: str) -> dict[str, any]:
    dtypes = {
        'group_session': str,
        'task': str,
        'station': str,
        'timestamp_iso8601': str,
        'timestamp_unix': float,
        'participant': int,
    }

    exp_df = pd.read_csv(csv_path, dtype=dtypes)
    lion_df = exp_df[exp_df['station'] == 'lion']
    tiger_df = exp_df[exp_df['station'] == 'tiger']
    leopard_df = exp_df[exp_df['station'] == 'leopard']

    assert lion_df["timestamp_unix"].is_monotonic_increasing
    assert tiger_df["timestamp_unix"].is_monotonic_increasing
    assert leopard_df["timestamp_unix"].is_monotonic_increasing

    exp_name = os.path.splitext(os.path.basename(csv_path))[0]

    return {
        'experiment_name': exp_name,
        'lion': lion_df,
        'tiger': tiger_df,
        'leopard': leopard_df
    }


def read_raw_csv_all(dir_path: str,
                     num_processes: int = 1,
                     whitelist_experiments: list[str] | None = None) -> list[dict[str, any]]:
    csv_paths = sorted(glob.glob(f'{dir_path}/*.csv'))

    if whitelist_experiments is not None:
        csv_paths = [path for path in csv_paths if os.path.splitext(os.path.basename(path))[0] in whitelist_experiments]

    if len(csv_paths) == 0:
        raise ValueError(f'No CSV files found')

    if num_processes == 1:
        raw_list = [read_raw_csv(csv_path) for csv_path in tqdm(csv_paths)]
    else:
        with Pool(processes=num_processes) as pool:
            raw_list = list(tqdm(pool.imap(read_raw_csv, csv_paths), total=len(csv_paths)))

    return raw_list
