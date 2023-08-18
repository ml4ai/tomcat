import os
from multiprocessing import Pool

import pandas as pd
from tqdm import tqdm


def write_csv(experiment: dict[str, any], dir_path: str):
    merged_df = None
    for station in ["lion", "tiger", "leopard"]:
        df = experiment[station]
        if merged_df is None:
            merged_df = df
        else:
            merged_df = pd.concat([merged_df, df], ignore_index=True)

    exp_name = experiment["experiment_name"]
    merged_df.to_csv(os.path.join(dir_path, f'{exp_name}.csv'), index=False)


def _write_experiment_csv(process_args: tuple[dict[str, any], str]):
    write_csv(*process_args)


def write_csv_all(experiments: list[dict[str, any]], dir_path: str, num_processes: int = 1):
    os.makedirs(dir_path, exist_ok=True)

    process_args = [(experiment, dir_path) for experiment in experiments]

    with Pool(processes=num_processes) as pool:
        for _ in tqdm(pool.imap(_write_experiment_csv, process_args), total=len(process_args)):
            pass
