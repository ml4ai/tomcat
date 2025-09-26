import os
from multiprocessing import Pool

import pandas as pd
from tqdm import tqdm
from sqlalchemy import create_engine
from config import POSTGRESQL_ENGINE


def write_db(experiment: dict[str, any], table_name: str):
    merged_df = None
    for station in ["lion", "tiger", "leopard"]:
        df = experiment[station]
        if merged_df is None:
            merged_df = df
        else:
            merged_df = pd.concat([merged_df, df], ignore_index=True)

    engine = create_engine(POSTGRESQL_ENGINE)
    merged_df.to_sql(table_name, engine, if_exists='append', index=False)


def _write_experiment_db(process_args: tuple[dict[str, any], str]):
    write_db(*process_args)


def write_db_all(experiments: list[dict[str, any]], table_name: str, num_processes: int = 1):
    process_args = [(experiment, table_name) for experiment in experiments]

    with Pool(processes=num_processes) as pool:
        for _ in tqdm(pool.imap(_write_experiment_db, process_args), total=len(process_args)):
            pass
