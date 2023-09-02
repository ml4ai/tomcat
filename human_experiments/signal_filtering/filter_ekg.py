from config import EKG_RAW_PATH, NUM_PROCESSES, EKG_FILTERED_PATH
from filter_signal import filter_signal_all, ekg_filter
from read_data import read_raw_csv_all
from write_data import write_csv_all

if __name__ == "__main__":
    experiments_ekg_raw = read_raw_csv_all(EKG_RAW_PATH, NUM_PROCESSES)
    experiments_ekg_filtered = filter_signal_all(experiments_ekg_raw, ekg_filter, NUM_PROCESSES)
    write_csv_all(experiments_ekg_filtered, EKG_FILTERED_PATH, NUM_PROCESSES)
