import argparse
from .df_to_csv import dataframe_to_csv
from .folder_name_manipulation import get_new_file_paths
from .create_time_distribution import create_time_distribution

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ("yes", "true", "t", "y", "1"):
        return True
    elif v.lower() in ("no", "false", "f", "n", "0"):
        return False
    else:
        raise argparse.ArgumentTypeError("Boolean value expected.")
    