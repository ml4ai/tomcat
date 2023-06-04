import argparse
from .read_NIRS import read_nirs
from .read_rest_state_timestamps import read_rest_state_timestamps
from .read_finger_tapping_time import read_finger_tapping_time
from .read_affective_task_timestamps import read_affective_task_timestamps_individual, read_affective_task_timestamps_team
from .read_ping_pong_timestamps import read_ping_pong_timestamps
from .read_minecraft_timestamps import read_minecraft_timestamps
from .create_time_distribution import create_time_distribution
from .NIRS_tasks_merge import NIRS_tasks_merge
from .create_time_distribution import create_time_distribution
from .label_data import label_data

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ("yes", "true", "t", "y", "1"):
        return True
    elif v.lower() in ("no", "false", "f", "n", "0"):
        return False
    else:
        raise argparse.ArgumentTypeError("Boolean value expected.")
    