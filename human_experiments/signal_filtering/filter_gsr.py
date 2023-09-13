from config import GSR_RAW_PATH, NUM_PROCESSES, GSR_FILTERED_PATH
from filter_signal import filter_signal_all, gsr_filter
from read_data import read_raw_csv_all
from write_data import write_csv_all

if __name__ == "__main__":
    experiments_gsr_raw = read_raw_csv_all(GSR_RAW_PATH, NUM_PROCESSES)
    experiments_gsr_filtered = filter_signal_all(experiments_gsr_raw, gsr_filter, NUM_PROCESSES)
    write_csv_all(experiments_gsr_filtered, GSR_FILTERED_PATH, NUM_PROCESSES)
