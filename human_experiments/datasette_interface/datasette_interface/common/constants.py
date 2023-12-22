import os

MISSING_INFO = "unknown"
DEFAULT_EXPERIMENTS_ROOT_DIR = os.getenv(
    "EXP_ROOT_DIR", "/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"
)
OPENSMILE_CONFIG_DIR = os.getenv("OPENSMILE_DIR", "config/opensmile")

# Directory to save files temporarily during a run.
TMP_DIR = os.getenv("TMP_DIR", os.path.abspath(".tmp"))
os.makedirs(TMP_DIR, exist_ok=True)

DEBUG = os.getenv("DEBUG", "0") == "1"
