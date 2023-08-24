import math
from multiprocessing import Pool

import numpy as np
import pandas as pd
from tqdm import tqdm

from .interpolation import linear_interpolation
from .resample import mne_resample
from .utils import get_shared_start_time, generate_time_series_num_samples


def synchronize_signals(experiment: dict[str, any]) -> dict[str, any]:
    desired_frequency = experiment["desired_freq"]
    experiment_name = experiment["experiment_name"]
    signals = experiment["signals"]

    signals_df = []
    for signal in signals:
        for station in ["lion", "tiger", "leopard"]:
            if station in signal:
                signals_df.append(signal[station])
    start_time = float(math.ceil(get_shared_start_time(signals_df)))

    processed_signals = {}
    for signal in signals:
        signal_frequency = signal["frequency"]
        for station in ["lion", "tiger", "leopard"]:
            if station in signal:
                signal_start_time = signal[station]["timestamp_unix"].min()

                # Resample signal
                resampled_signal = mne_resample(
                    signal[station].drop(columns=["timestamp_unix"]),
                    signal_frequency, desired_frequency
                )

                # Assign timestamp for interpolation
                resampled_signal["timestamp_unix"] = generate_time_series_num_samples(
                    signal_start_time, len(resampled_signal), desired_frequency
                )

                try:
                    # Interpolate signal to synchronize with other signals
                    interpolated_signal = linear_interpolation(
                        resampled_signal, desired_frequency, start_time
                    )
                except AssertionError as e:
                    print(f"{experiment_name} - {station} ERROR")
                    raise e

                # Drop current timestamp to use the unified timestamps later
                interpolated_signal = interpolated_signal.drop(columns=["timestamp_unix"])

                signal_type = signal["signal_type"]
                processed_signals[f"{station}_{signal_type}"] = interpolated_signal

    # Align signals
    max_length = max(len(df) for df in processed_signals.values())

    ready_for_synchronization_signals = []
    for name, processed_signal in processed_signals.items():
        # Reindex the dataframes to the maximum length to pad with NaNs at the end
        processed_signal = processed_signal.reindex(np.arange(max_length))
        # Add prefix to each column name in the dataframes
        processed_signal = processed_signal.add_prefix(f"{name}_")

        ready_for_synchronization_signals.append(processed_signal)

    # Concatenate the dataframes column-wise
    synchronized_signal = pd.concat(ready_for_synchronization_signals, axis=1)

    time_series_frequency = desired_frequency

    # Assign timestamp
    synchronized_signal["timestamp_unix"] = generate_time_series_num_samples(
        start_time,
        len(synchronized_signal),
        time_series_frequency
    )

    # Move timestamp_unix column to the first column
    columns = ['timestamp_unix'] + [col for col in synchronized_signal.columns if col != 'timestamp_unix']
    synchronized_signal = synchronized_signal[columns]

    synchronization_results = {
        "experiment_name": experiment_name,
        "signals": synchronized_signal
    }

    return synchronization_results


def synchronize_signals_all(experiments: list[dict[str, any]],
                            num_processes: int = 1) -> list[dict[str, any]]:
    if num_processes > 1:
        with Pool(processes=num_processes) as pool:
            synchronized_list = list(tqdm(pool.imap(synchronize_signals, experiments), total=len(experiments)))
    else:
        synchronized_list = []
        for experiment in tqdm(experiments):
            synchronized_list.append(synchronize_signals(experiment))

    return synchronized_list
