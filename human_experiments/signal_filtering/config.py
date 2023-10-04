import os

USER = os.getenv("USER")
DB_PATH = f"/space/{USER}/tomcat/tomcat.db"
EEG_RAW_PATH = f"/space/{USER}/eeg_raw"
EKG_RAW_PATH = f"/space/{USER}/ekg_raw"
GSR_RAW_PATH = f"/space/{USER}/gsr_raw"
EEG_FILTERED_PATH = f"/space/{USER}/eeg_filtered"
EKG_FILTERED_PATH = f"/space/{USER}/ekg_filtered"
GSR_FILTERED_PATH = f"/space/{USER}/gsr_filtered"
FNIRS_FILTERED_PATH = f"/space/{USER}/fnirs_filtered"
POSTGRESQL_ENGINE = f"postgresql://{USER}@localhost:5433/tomcat"

NUM_PROCESSES = 40

EXPERIMENT_SESSIONS = [
    "exp_2022_09_30_10", "exp_2022_11_01_10", "exp_2022_12_02_15", "exp_2023_02_21_14",
    "exp_2022_10_04_09", "exp_2022_11_04_10", "exp_2022_12_05_12", "exp_2023_04_17_13",
    "exp_2022_10_07_15", "exp_2022_11_07_10", "exp_2023_01_30_13", "exp_2023_04_18_14",
    "exp_2022_10_14_10", "exp_2022_11_08_11", "exp_2023_01_31_14", "exp_2023_04_21_10",
    "exp_2022_10_18_10", "exp_2022_11_10_10", "exp_2023_02_03_10", "exp_2023_04_24_13",
    "exp_2022_10_21_15", "exp_2022_11_14_12", "exp_2023_02_06_13", "exp_2023_04_27_14",
    "exp_2022_10_24_12", "exp_2022_11_15_13", "exp_2023_02_07_14", "exp_2023_04_28_10",
    "exp_2022_10_27_10", "exp_2022_11_17_15", "exp_2023_02_10_10", "exp_2023_05_01_13",
    "exp_2022_10_28_10", "exp_2022_11_18_10", "exp_2023_02_16_14", "exp_2023_05_02_14",
    "exp_2022_10_31_10", "exp_2022_11_22_10", "exp_2023_02_20_01", "exp_2023_05_03_10",
]

COMMON_COLUMNS = ['group_session', 'station', 'participant', 'id', 'task', 'timestamp_unix', 'timestamp_iso8601']
EEG_COLUMNS = ['aff1h', 'aff5h', 'f7', 'fc5', 'fc1', 'c3', 't7', 'tp9', 'cp5', 'cp1',
               'pz', 'p3', 'p7', 'po9', 'o1', 'oz', 'o2', 'po10', 'p8', 'p4', 'tp10',
               'cp6', 'cp2', 'cz', 'c4', 't8', 'fc6', 'fc2', 'fcz', 'f8', 'aff6h', 'aff2h']
EKG_COLUMNS = ['aux_ekg']
GSR_COLUMNS = ['aux_gsr']
