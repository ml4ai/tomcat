import logging
import os
import sys

from common import remove_columns_all_exp
from config import EEG_FILTERED_PATH, NUM_PROCESSES, EXPERIMENT_SESSIONS, OUTPUT_DIR
from read_data import read_raw_csv_all
from signal_synchronization import prepare_synchronization_data, synchronize_signals_all
from task_synchronization import synchronize_task_signal_all, prepare_task_synchronization_data
from write_data import write_csv_all, write_signal_csv_all

if __name__ == "__main__":
    upsample_frequency = 2000
    desired_freq = 500

    output_dir = os.path.join(OUTPUT_DIR, f"eeg_{desired_freq}hz")
    os.makedirs(output_dir, exist_ok=True)

    logging_file = os.path.join(output_dir, "log.txt")
    logging.basicConfig(
        level=logging.INFO,
        handlers=(
            logging.FileHandler(
                filename=logging_file, mode="w"
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    print("Reading data...")
    experiment_eeg_signal = read_raw_csv_all(EEG_FILTERED_PATH, NUM_PROCESSES, EXPERIMENT_SESSIONS)

    columns_to_remove = [
        "group_session",
        "station",
        "task",
        "participant",
        "timestamp_iso8601"
    ]

    remove_columns_all_exp(experiment_eeg_signal, columns_to_remove)

    signal_type_info = [
        {
            "signal_type": "eeg",
            "experiment_signals": experiment_eeg_signal,
            "recording_frequency": 500,
        }
    ]

    print("Synchronizing signals...")
    sync_experiments_info = prepare_synchronization_data(signal_type_info, upsample_frequency, desired_freq)
    synchronized_signals = synchronize_signals_all(sync_experiments_info)

    print("Writing synchronized signals...")
    output_dir = os.path.join(OUTPUT_DIR, f"eeg_{desired_freq}hz")
    write_signal_csv_all(synchronized_signals, output_dir, NUM_PROCESSES)

    print("Synchronizing task signals...")
    task_synchronization_info = prepare_task_synchronization_data(synchronized_signals, NUM_PROCESSES)
    synchronized_task_signals = synchronize_task_signal_all(task_synchronization_info)

    print("Writing synchronized signals and tasks...")
    write_csv_all(synchronized_task_signals, output_dir, NUM_PROCESSES)
