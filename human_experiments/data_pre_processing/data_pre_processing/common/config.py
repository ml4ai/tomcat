import os

USER = os.getenv("USER")

EXP_DIR = "/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"
OUT_DIR = f"/space/{USER}/tomcat/preprocessed"
LOG_DIR = f"/space/{USER}/tomcat"

NUM_PROCESSES = 40
