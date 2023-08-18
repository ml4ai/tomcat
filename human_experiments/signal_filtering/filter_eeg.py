from config import EEG_RAW_PATH, NUM_PROCESSES, EEG_FILTERED_PATH
from filter_signal import filter_signal_all, eeg_filter
from read_data import read_raw_csv_all
from write_data import write_csv_all

if __name__ == "__main__":
    experiments_eeg_raw = read_raw_csv_all(EEG_RAW_PATH, NUM_PROCESSES)
    experiments_eeg_filtered = filter_signal_all(experiments_eeg_raw, eeg_filter, NUM_PROCESSES)
    write_csv_all(experiments_eeg_filtered, EEG_FILTERED_PATH, NUM_PROCESSES)
