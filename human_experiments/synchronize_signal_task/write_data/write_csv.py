import os
from multiprocessing import Pool

from tqdm import tqdm


def write_csv(experiment: dict[str, any], dir_path: str):
    experiment_name = experiment["experiment_name"]
    output_dir_path = os.path.join(dir_path, experiment_name)
    os.makedirs(output_dir_path, exist_ok=True)

    for task, df in experiment.items():
        if task == "experiment_name":
            continue
        df.to_csv(os.path.join(output_dir_path, f'{task}.csv'), index=False)


def _write_experiment_csv(process_args: tuple[dict[str, any], str]):
    write_csv(*process_args)


def write_csv_all(experiments: list[dict[str, any]], dir_path: str, num_processes: int = 1):
    os.makedirs(dir_path, exist_ok=True)

    process_args = [(experiment, dir_path) for experiment in experiments]

    with Pool(processes=num_processes) as pool:
        for _ in tqdm(pool.imap(_write_experiment_csv, process_args), total=len(process_args)):
            pass
