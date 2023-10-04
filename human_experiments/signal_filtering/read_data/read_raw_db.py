from multiprocessing import Pool

import pandas as pd
from sqlalchemy import create_engine
from tqdm import tqdm

from config import EXPERIMENT_SESSIONS, COMMON_COLUMNS, EEG_COLUMNS, EKG_COLUMNS, GSR_COLUMNS, POSTGRESQL_ENGINE

from enum import Enum


class Modality(Enum):
    EEG = 1
    EKG = 2
    GSR = 3


def read_raw_db(table_name: str,
                group_session: str,
                modality: Modality | None = None) -> dict[str, any]:
    if modality is None:
        columns_str = '*'
    else:
        columns = COMMON_COLUMNS
        match modality:
            case Modality.EEG:
                columns += EEG_COLUMNS
            case Modality.EKG:
                columns += EKG_COLUMNS
            case Modality.GSR:
                columns += GSR_COLUMNS
            case _:
                raise ValueError("Invalid modality")

        columns_str = ', '.join(columns)

    query = f"""
            SELECT {columns_str}
            FROM {table_name}
            WHERE group_session = '{group_session}';
            """
    engine = create_engine(POSTGRESQL_ENGINE)
    exp_df = pd.read_sql_query(query, engine)

    lion_df = exp_df[exp_df['station'] == 'lion'].sort_values("timestamp_unix", ascending=True)
    tiger_df = exp_df[exp_df['station'] == 'tiger'].sort_values("timestamp_unix", ascending=True)
    leopard_df = exp_df[exp_df['station'] == 'leopard'].sort_values("timestamp_unix", ascending=True)

    assert lion_df["timestamp_unix"].is_monotonic_increasing
    assert tiger_df["timestamp_unix"].is_monotonic_increasing
    assert leopard_df["timestamp_unix"].is_monotonic_increasing

    exp_data = {
        "experiment_name": group_session,
        "lion": lion_df,
        "tiger": tiger_df,
        "leopard": leopard_df
    }

    return exp_data


def _multiprocess_read_raw_db(process_arg):
    return read_raw_db(*process_arg)


def read_raw_db_all(table_name: str,
                    num_processes: int = 1,
                    modality: Modality | None = None,
                    blacklist_experiments: list[str] | None = None) -> list[dict[str, any]]:
    # Hard coded EXPERIMENT_SESSIONS because querying for experiments is slow
    group_sessions = EXPERIMENT_SESSIONS if blacklist_experiments is None \
        else list(set(EXPERIMENT_SESSIONS) - set(blacklist_experiments))

    process_args = [(table_name, group_session, modality) for group_session in group_sessions]

    with Pool(processes=num_processes) as pool:
        raw_list = list(tqdm(pool.imap(_multiprocess_read_raw_db, process_args), total=len(process_args)))

    return raw_list
