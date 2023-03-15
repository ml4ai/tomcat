import argparse

import os

import pandas as pd
from tqdm import tqdm

from human_experiments.data_pre_processing.summary.experiment_summary import ExperimentSummary


def generate_summary(experiments_dir: str, out_dir: str):
    experiment_directories = list(os.listdir(experiments_dir))

    dfs = []
    i = 0
    for experiment_dir in tqdm(experiment_directories, total=len(experiment_directories), desc="Experiments"):
        dfs.append(ExperimentSummary.from_experiment_directory(experiment_dir).to_data_frame())
        i += 1
        if i == 10:
            break

    summary_df = pd.concat(dfs)

    summary_df.to_csv(f"{out_dir}/summary.csv")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Parses a collection of experiments and generates a single .csv file with the most relevant "
                    "information from each one of them."
    )

    parser.add_argument("--experiments_dir", type=str, required=True,
                        help="Directory containing experiment folders.")
    parser.add_argument("--out_dir", type=str, required=True,
                        help="Directory where the .csv file containing a summary of the experiments must be saved.")

    args = parser.parse_args()

    generate_summary(args.experiments_dir, args.out_dir)