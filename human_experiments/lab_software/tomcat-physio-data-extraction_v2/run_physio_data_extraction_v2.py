'''
This script is used to extract the physio data from the XDF files for new data pipeline. 
'''

import os
import sys
import pyxdf
import shutil
import argparse
import numpy as np
from termcolor import colored
from utils import (
    str2bool,
    read_nirs,
    read_eeg,
    read_gaze,
    read_rest_state_timestamps, 
    read_finger_tapping_time,
    read_affective_task_timestamps_individual,
    read_affective_task_timestamps_team,
    read_ping_pong_timestamps,
    read_minecraft_timestamps,
    tasks_merge,
    label_data,
    save_NIRS,
    save_EEG,
    save_Gaze,
)

def read_xdf(
    xdf_file_paths,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    exclude,
    filter,
    output_path,
    rootdir_xdf
):
    """
    Read the XDF files.
    """
    columns = shutil.get_terminal_size().columns
    for path in xdf_file_paths:
        if "block_1" in path:
            print(
                    colored("block_1 ", "magenta", attrs=["bold", "blink"]).center(columns)
                )
            block_1, _ = pyxdf.load_xdf(path)
            
            lion_0297_block_1_NIRS, tiger_0239_block_1_NIRS, leopard_0171_block_1_NIRS, lion_0297_raw_w1, tiger_0239_raw_w1, leopard_0171_raw_w1  = read_nirs(block_1) # 1.1 Read NIRS timerseries data and its timestamps

            lion_0297_block_1_EEG, tiger_0239_block_1_EEG, leopard_0171_block_1_EEG = read_eeg(block_1) # 1.2 Read EEG timerseries data and its timestamps

            lion_0297_block_1_Gaze, tiger_0239_block_1_Gaze, leopard_0171_block_1_Gaze = read_gaze(block_1) # 1.3 Read Gaze timerseries data and its timestamps

            rest_state_marker = read_rest_state_timestamps(block_1) # 2. Read RestState timestamps
            finger_tapping_marker = read_finger_tapping_time(block_1, rest_state_marker) # 3. Read FingerTapping timestamps
            AffectiveTask_individual_marker = read_affective_task_timestamps_individual(block_1, finger_tapping_marker) # 4. Read AffectiveTask timestamps
            AffectiveTask_team_marker = read_affective_task_timestamps_team(block_1, AffectiveTask_individual_marker) # 5. Read AffectiveTask timestamps
            PingPong_markers = read_ping_pong_timestamps(block_1, AffectiveTask_team_marker) # 6. Read PingPong timestamps   

        elif "block_2" in path:
            print(
                    colored("block_2 ", "magenta", attrs=["bold", "blink"]).center(columns)
                )
            block_2, _ = pyxdf.load_xdf(path)
            lion_0297_block_2_NIRS, tiger_0239_block_2_NIRS, leopard_0171_block_2_NIRS, _, _, _  = read_nirs(block_2) # 1.1 Read NIRS data

            lion_0297_block_2_EEG, tiger_0239_block_2_EEG, leopard_0171_block_2_EEG = read_eeg(block_2) # 1.2 Read EEG data

            lion_0297_block_2_Gaze, tiger_0239_block_2_Gaze, leopard_0171_block_2_Gaze = read_gaze(block_2) # 1.3 Read Gaze data

            minecraft_markers = read_minecraft_timestamps(block_2, PingPong_markers) # 2. Read Minecraft timestamps
    
    # Merge NIRS block 1 and block 2
    lion_0297_block_NIRS, tiger_0239_block_NIRS, leopard_0171_block_NIRS = tasks_merge(lion_0297_block_1_NIRS, tiger_0239_block_1_NIRS, leopard_0171_block_1_NIRS, lion_0297_block_2_NIRS, tiger_0239_block_2_NIRS, leopard_0171_block_2_NIRS)

    # Merge EEG block 1 and block 2
    lion_0297_block_EEG, tiger_0239_block_EEG, leopard_0171_block_EEG = tasks_merge(lion_0297_block_1_EEG, tiger_0239_block_1_EEG, leopard_0171_block_1_EEG, lion_0297_block_2_EEG, tiger_0239_block_2_EEG, leopard_0171_block_2_EEG)

    # Merge Gaze block 1 and block 2
    lion_0297_block_Gaze, tiger_0239_block_Gaze, leopard_0171_block_Gaze = tasks_merge(lion_0297_block_1_Gaze, tiger_0239_block_1_Gaze, leopard_0171_block_1_Gaze, lion_0297_block_2_Gaze, tiger_0239_block_2_Gaze, leopard_0171_block_2_Gaze)

    # Label the NIRS data with tasks timestamps: RestState, FingerTapping, AffectiveTask, PingPong, Minecraft
    lion_0297_block_NIRS_labeled, tiger_0239_block_NIRS_labeled, leopard_0171_block_NIRS_labeled = label_data(lion_0297_block_NIRS, tiger_0239_block_NIRS, leopard_0171_block_NIRS, minecraft_markers)

    # Label the EEG data with tasks timestamps: RestState, FingerTapping, AffectiveTask, PingPong, Minecraft
    lion_0297_block_EEG_labeled, tiger_0239_block_EEG_labeled, leopard_0171_block_EEG_labeled = label_data(lion_0297_block_EEG, tiger_0239_block_EEG, leopard_0171_block_EEG, minecraft_markers)

    # Label the Gaze data with tasks timestamps: RestState, FingerTapping, AffectiveTask, PingPong, Minecraft
    lion_0297_block_Gaze_labeled, tiger_0239_block_Gaze_labeled, leopard_0171_block_Gaze_labeled = label_data(lion_0297_block_Gaze, tiger_0239_block_Gaze, leopard_0171_block_Gaze, minecraft_markers)
    
    #Filter and save the NIRS data
    save_NIRS(lion_0297_block_NIRS_labeled, tiger_0239_block_NIRS_labeled, leopard_0171_block_NIRS_labeled, lion_0297_raw_w1, tiger_0239_raw_w1, leopard_0171_raw_w1 ,rootdir_xdf, output_path, extract_pkl, extract_csv, extract_hdf5, filter)
    
    #Filter and save the EEG data
    save_EEG(lion_0297_block_EEG_labeled, tiger_0239_block_EEG_labeled, leopard_0171_block_EEG_labeled, rootdir_xdf, output_path, extract_pkl, extract_csv, extract_hdf5, filter)

    #Filter and save the Gaze data
    save_Gaze(lion_0297_block_Gaze_labeled, tiger_0239_block_Gaze_labeled, leopard_0171_block_Gaze_labeled, rootdir_xdf, output_path, extract_pkl, extract_csv, extract_hdf5, filter)

def look_for_XDF_files(
    rootdir_xdf,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    exclude,
    filter,
    output_path,
):
    """
    Walk through root directory, looking for the xdf files.
    """
    xdf_file_paths = [
    os.path.join(root, file)
    for root, dirs, files in os.walk(rootdir_xdf)
    for file in files
    if file.endswith(".xdf")
    ]
    
    for file_path in xdf_file_paths:
        print(
            colored("[Status] xdf file found at ", "green", attrs=["bold"]),
            colored(file_path, "blue"),
        )


    read_xdf(
        sorted(xdf_file_paths),
        extract_pkl,
        extract_csv,
        extract_hdf5,
        exclude,
        filter,
        output_path,
        rootdir_xdf,
    )  # 1. read all the XDF files


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Post experiment script for xdf to csv file conversion"
    )
    parser.add_argument(
        "--p", required=True, help="Path to the directory with the XDF files"
    )

    parser.add_argument(
        "--pkl",
        default=False,
        type=str2bool,
        help="By setting pkl to True you extract xdf files as pickle file",
    )

    parser.add_argument(
        "--csv",
        default=True,
        type=str2bool,
        help="By setting csv to True you extract xdf files as csv file",
    )

    parser.add_argument(
        "--hdf5",
        default=False,
        type=str2bool,
        help="By setting hdf5 to True you extract xdf files as hdf5 file",
    )

    parser.add_argument(
        "--exclude",
        required=False,
        default=None,
        help="Enter iMAC name you'd like to exclude",
    )

    parser.add_argument(
        "--filter",
        required=False,
        default=None,
        help="Enter True if you want to filter the signal",
    )

    parser.add_argument(
        "--output_path",
        required=False,
        default=None,
        help="Enter path to a folder where you want to extract the physio data, by default \
            it will go to same folder your reading XDF files from",
    )

    arg = parser.parse_args()

    rootdir_xdf = arg.p
    extract_pkl = arg.pkl
    extract_hdf5 = arg.hdf5
    extract_csv = arg.csv
    exclude = str(arg.exclude)
    filter = str(arg.filter)
    output_path = arg.output_path

    print(
        colored("[Status] Root Directory:", "green", attrs=["bold"]),
        colored(rootdir_xdf, "blue"),
    )
    sys.exit(
        look_for_XDF_files(
            rootdir_xdf,
            extract_pkl,
            extract_csv,
            extract_hdf5,
            exclude,
            filter,
            output_path,
        )
    )
