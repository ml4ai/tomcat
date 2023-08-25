import os

USER = os.getenv("USER")
DB_PATH = f"/space/{USER}/tomcat/tomcat.db"
EEG_FILTERED_PATH = f"/space/{USER}/eeg_filtered"
FNIRS_FILTERED_PATH = f"/space/{USER}/fnirs_filtered"
NUM_PROCESSES = 40
OUTPUT_DIR = "/tomcat/data/derived/drafts/release_2023_08_23_13"

MINECRAFT_MISSION_BLACKLIST = [
    "560d4c45-dc45-4e19-bdb3-e4e15021728a",
    "a48f475f-40b0-46b9-8284-0db267dddb67",
    "171a8713-a554-4d8e-a4b1-3ec1b728d0a2",
    "9cde1985-1179-4aac-8b67-1fc60ed65243",
]

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
