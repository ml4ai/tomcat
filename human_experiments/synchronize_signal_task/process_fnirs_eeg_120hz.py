import os

from common import remove_columns_all_exp
from config import DB_PATH, FNIRS_FILTERED_PATH, NUM_PROCESSES, EXPERIMENT_SESSIONS, OUTPUT_DIR, EEG_FILTERED_PATH
from read_data import read_raw_csv_all
from signal_synchronization import prepare_synchronization_data, synchronize_signals_all
from task_synchronization import synchronize_task_signal_all, prepare_task_synchronization_data
from write_data import write_csv_all

if __name__ == "__main__":
    desired_freq = 120

    experiment_fnirs_signal = read_raw_csv_all(FNIRS_FILTERED_PATH, NUM_PROCESSES, EXPERIMENT_SESSIONS)
    experiment_eeg_signal = read_raw_csv_all(EEG_FILTERED_PATH, NUM_PROCESSES, EXPERIMENT_SESSIONS)

    columns_to_remove = [
        "group_session",
        "station",
        "task",
        "participant",
        "timestamp_iso8601"
    ]

    remove_columns_all_exp(experiment_fnirs_signal, columns_to_remove)
    remove_columns_all_exp(experiment_eeg_signal, columns_to_remove)

    signal_type_info = [
        {
            "signal_type": "fnirs",
            "experiment_signals": experiment_fnirs_signal,
            "recording_frequency": 10,
        },
        {
            "signal_type": "eeg",
            "experiment_signals": experiment_eeg_signal,
            "recording_frequency": 500,
        }
    ]

    sync_experiments_info = prepare_synchronization_data(signal_type_info, desired_freq)
    synchronized_signals = synchronize_signals_all(sync_experiments_info)

    task_synchronization_info = prepare_task_synchronization_data(synchronized_signals, DB_PATH, NUM_PROCESSES)
    synchronized_task_signals = synchronize_task_signal_all(task_synchronization_info)

    output_dir = os.path.join(OUTPUT_DIR, f"fnirs_eeg_{desired_freq}hz")
    write_csv_all(synchronized_task_signals, output_dir, NUM_PROCESSES)
