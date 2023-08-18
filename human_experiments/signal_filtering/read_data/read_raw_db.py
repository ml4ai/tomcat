import sqlite3
from multiprocessing import Pool

import pandas as pd
from tqdm import tqdm

from config import EXPERIMENT_SESSIONS


def read_raw_db(db_path: str,
                table_name: str,
                group_session: str) -> dict[str, any]:
    db_connection = sqlite3.connect(db_path)

    query = f"""
        SELECT * 
        FROM {table_name}
        WHERE group_session = ? AND station = ?;
    """

    exp_data = {"experiment_name": group_session}
    for station in ["lion", "tiger", "leopard"]:
        station_df = pd.read_sql_query(query, db_connection, params=[group_session, station])
        exp_data[station] = station_df

    return exp_data


def _multiprocess_read_raw_db(process_arg):
    return read_raw_db(*process_arg)


def read_raw_db_all(db_path: str,
                    table_name: str,
                    num_processes: int = 1,
                    whitelist_experiments: list[str] | None = None) -> list[dict[str, any]]:
    # Hard coded EXPERIMENT_SESSIONS because querying for experiments is slow
    group_sessions = EXPERIMENT_SESSIONS if whitelist_experiments is None else whitelist_experiments

    process_args = [(db_path, table_name, group_session) for group_session in group_sessions]

    with Pool(processes=num_processes) as pool:
        raw_list = list(tqdm(pool.imap(_multiprocess_read_raw_db, process_args), total=len(process_args)))

    return raw_list
