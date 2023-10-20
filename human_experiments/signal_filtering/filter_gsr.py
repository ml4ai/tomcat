from config import NUM_PROCESSES, GSR_FILTERED_PATH
from filter_signal import filter_signal_all, gsr_filter
from read_data import read_raw_db_all, Modality
from write_data import write_db_all, write_csv_all

if __name__ == "__main__":
    blacklist_experiment = ["exp_2022_10_24_12"]
    experiments_gsr_raw = read_raw_db_all("eeg_raw_tmp", NUM_PROCESSES, Modality.GSR, blacklist_experiment)
    experiments_gsr_filtered = filter_signal_all(experiments_gsr_raw, gsr_filter, NUM_PROCESSES)
    write_csv_all(experiments_gsr_filtered, GSR_FILTERED_PATH, NUM_PROCESSES)
    # write_db_all(experiments_gsr_filtered, "gsr_filtered", NUM_PROCESSES)
