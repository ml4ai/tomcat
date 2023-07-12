import argparse
from .old_data_acqusition_pipeline.df_to_csv import dataframe_to_csv_1
from .old_data_acqusition_pipeline.folder_name_manipulation import get_new_file_paths
from .old_data_acqusition_pipeline.create_time_distribution import create_time_distribution
from .old_data_acqusition_pipeline.read_xdf import read_xdf as read_xdf_old

from .new_data_acqusition_pipeline.read_xdf import read_xdf as read_xdf_new
from .new_data_acqusition_pipeline.read_EEG import read_eeg
from .new_data_acqusition_pipeline.read_NIRS import read_nirs
from .new_data_acqusition_pipeline.read_Gaze import read_gaze
from .new_data_acqusition_pipeline.create_baseline_task_directory import create_baseline_task_directory
from .new_data_acqusition_pipeline.read_rest_state_timestamps import read_rest_state_timestamps
from .new_data_acqusition_pipeline.read_finger_tapping_time import read_finger_tapping_time
from .new_data_acqusition_pipeline.read_affective_task_timestamps import read_affective_task_timestamps_individual, read_affective_task_timestamps_team
from .new_data_acqusition_pipeline.read_ping_pong_timestamps import read_ping_pong_timestamps
from .new_data_acqusition_pipeline.save_EEG import save_EEG
from .new_data_acqusition_pipeline.save_NIRS import save_NIRS
from .new_data_acqusition_pipeline.save_Gaze import save_Gaze
from .new_data_acqusition_pipeline.tasks_merge import tasks_merge

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ("yes", "true", "t", "y", "1"):
        return True
    elif v.lower() in ("no", "false", "f", "n", "0"):
        return False
    else:
        raise argparse.ArgumentTypeError("Boolean value expected.")
    