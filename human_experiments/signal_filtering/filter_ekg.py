from config import NUM_PROCESSES, EKG_FILTERED_PATH
from filter_signal import filter_signal_all, ekg_filter
from read_data import read_raw_db_all, Modality
from write_data import write_db_all, write_csv_all

if __name__ == "__main__":
    blacklist_experiment = ["exp_2022_10_24_12"]
    experiments_ekg_raw = read_raw_db_all("eeg_raw", NUM_PROCESSES, Modality.EKG, blacklist_experiment)
    experiments_ekg_filtered = filter_signal_all(experiments_ekg_raw, ekg_filter, NUM_PROCESSES)
    write_csv_all(experiments_ekg_filtered, EKG_FILTERED_PATH, NUM_PROCESSES)
    # write_db_all(experiments_ekg_filtered, "ekg_filtered", NUM_PROCESSES)
