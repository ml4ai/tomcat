from config import NUM_PROCESSES, FNIRS_FILTERED_PATH
from filter_signal import filter_signal_all, fnirs_filter
from read_data import read_raw_db_all
from write_data import write_db_all, write_csv_all

if __name__ == "__main__":
    experiments_fnirs_raw = read_raw_db_all("fnirs_raw", NUM_PROCESSES)
    print(experiments_fnirs_raw[0]["lion"].columns)
    experiments_fnirs_filtered = filter_signal_all(experiments_fnirs_raw, fnirs_filter, NUM_PROCESSES)
    write_csv_all(experiments_fnirs_filtered, FNIRS_FILTERED_PATH, NUM_PROCESSES)
    # write_db_all(experiments_fnirs_filtered, "fnirs_filtered", NUM_PROCESSES)
