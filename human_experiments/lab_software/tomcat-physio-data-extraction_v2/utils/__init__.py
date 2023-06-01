import argparse
from .read_NIRS import read_nirs
from .read_rest_state_timestamps import read_rest_state_timestamps
from .read_finger_tapping_time import read_finger_tapping_time

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ("yes", "true", "t", "y", "1"):
        return True
    elif v.lower() in ("no", "false", "f", "n", "0"):
        return False
    else:
        raise argparse.ArgumentTypeError("Boolean value expected.")
    