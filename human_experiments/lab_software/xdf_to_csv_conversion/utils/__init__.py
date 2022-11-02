import argparse
from .xdf_timestamps import get_start_stop_time_from_xdf
from .baseline_tasks_timestamps import read_baseline_tasks_time
from .minecraft_timestamps import  read_minecraft_time
from .create_write_csv_file import dataframe_to_csv
from .create_time_distribution import create_time_distribution
from .convert_datetime_timezone import change_time_zone

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')
