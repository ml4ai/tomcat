import os
import sys
import logging

USER = os.getenv("USER")

os.makedirs(f"/space/{USER}/tomcat", exist_ok=True)
DB_PATH = f"/space/{USER}/tomcat/tomcat.db"
LOG_FILE_PATH = f"/space/{USER}/tomcat/build_database.log"
FILE_HANDLER = logging.FileHandler(filename=LOG_FILE_PATH, mode="w")
STDERR_HANDLER = logging.StreamHandler(stream=sys.stderr)
logging_handlers = [FILE_HANDLER, STDERR_HANDLER]
NUM_PROCESSES = 40

EEG_IGNORE_EXPERIMENTS = [
    "exp_2022_10_24_12"  # this experiment has broken EEG signal timestamps
]
