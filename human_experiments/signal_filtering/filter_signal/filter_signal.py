from multiprocessing import Pool

import mne
import pandas as pd
from tqdm import tqdm


def filter_signal(signal_df: pd.DataFrame, filter_method: callable):
    # Set the log level to 'ERROR' to suppress informational messages
    mne.set_log_level('ERROR')

    columns_to_ignore = [
        "group_session",
        "task",
        "station",
        "participant",
        "timestamp_unix",
        "timestamp_iso8601"
    ]

    # Iterate through the columns
    for column in signal_df.columns:
        # Check if the column is not in the ignore list
        if column not in columns_to_ignore:
            # Apply the filter method to the column
            signal_df[column] = filter_method(signal_df[column].to_numpy())


def _filter_signal_per_exp(process_arg: tuple[dict[str, any], callable]) -> dict[str, any]:
    experiment, filter_method = process_arg

    for station in ["lion", "tiger", "leopard"]:
        signal_df = experiment[station]
        if len(signal_df) == 0:
            continue
        filter_signal(signal_df, filter_method)
        experiment[station] = signal_df

    return experiment


def filter_signal_all(signals: list[dict[str, any]],
                      filter_method: callable,
                      num_processes: int = 1) -> list[dict[str, any]]:
    process_args = [(signal, filter_method) for signal in signals]

    with Pool(processes=num_processes) as pool:
        filtered_signals = list(tqdm(pool.imap(_filter_signal_per_exp, process_args), total=len(process_args)))

    return filtered_signals
