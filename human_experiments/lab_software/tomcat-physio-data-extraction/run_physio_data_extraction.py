import os
import sys
import pyxdf
import shutil
import argparse
import numpy as np
from termcolor import colored
from utils import (
    get_start_stop_time_from_xdf,
    dataframe_to_csv,
    create_time_distribution,
    str2bool,
)

def read_xdf(
    xdf_file_paths,
    rootdir_baseline_task,
    rootdir_minecraft_data,
    subject_id,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    exclude,
    filter,
    output_path,
):
    """
    Read the XDF files.
    """
    columns = shutil.get_terminal_size().columns
    for path in xdf_file_paths:
        data, header = pyxdf.load_xdf(path)

        if exclude not in path:
            if "lion" in path:
                print(
                    colored("Lion ", "magenta", attrs=["bold", "blink"]).center(columns)
                )
            elif "leopard" in path:
                print(
                    colored("Leopard ", "magenta", attrs=["bold", "blink"]).center(
                        columns
                    )
                )
            else:
                print(
                    colored(
                        "Tiger ", "magenta", "on_blue", attrs=["bold", "blink"]
                    ).center(columns)
                )

            for i in range(0, len(data)):
                if data[i]["info"]["type"] == ["NIRS"]:
                    print(
                        colored("[Status] Reading ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )
                    (
                        time_start_streams_nirs,
                        time_end_streams_nirs,
                    ) = get_start_stop_time_from_xdf(
                        data[i]
                    )  # get the unix time
                    (
                        time_distribution_human_readable_nirs,
                        time_distribution_unix_nirs,
                    ) = create_time_distribution(
                        data[i],
                    )
                    dataframe_to_csv(
                        path,
                        data[i]["time_series"],
                        "NIRS",
                        time_distribution_human_readable_nirs,
                        time_distribution_unix_nirs,
                        rootdir_baseline_task,
                        rootdir_minecraft_data,
                        subject_id,
                        extract_pkl,
                        extract_csv,
                        extract_hdf5,
                        filter,
                        output_path,
                    )

                elif data[i]["info"]["type"] == ["Markers"]:
                    # Our experiments don't use physical marker for the physio data
                    print(
                        colored("[Status] Skipping ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )

                elif data[i]["info"]["type"] == ["EEG"]:
                    print(colored("[Status] Reading ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )
                    time_start_streams_eeg, time_end_streams_eeg = get_start_stop_time_from_xdf(data[i])
                    time_distribution_human_readable_eeg, time_distribution_unix_eeg = create_time_distribution(data[i])
                    
                    # Create a list of channel names
                    EEG_channels = []
                    for channel_dict in data[i]['info']['desc'][0]['channels'][0]['channel']:
                        EEG_channels.append(channel_dict['label'][0])

                    channels_used = [
                        "AFF1h", "F7", "FC5", "C3", "T7", "TP9", "Pz", "P3", "P7", "O1", "O2", "P8",
                        "P4", "TP10", "Cz", "C4", "T8", "FC6", "FCz", "F8", "AFF2h", "AUX_GSR", "AUX_EKG"
                    ]

                    exclude_channels = [(i, ch) for i, ch in enumerate(EEG_channels) if ch not in channels_used]
                    exclude_indices = [index for index, _ in exclude_channels]
                    EEG_data = np.delete(data[i]['time_series'].T, exclude_indices, axis=0)

                    dataframe_to_csv(
                        path,
                        EEG_data.T,  # Use EEG data with channels we want
                        "EEG",
                        time_distribution_human_readable_eeg,
                        time_distribution_unix_eeg,
                        rootdir_baseline_task,
                        rootdir_minecraft_data,
                        subject_id,
                        extract_pkl,
                        extract_csv,
                        extract_hdf5,
                        filter,
                        output_path
                    )


                elif data[i]["info"]["type"] == ["Gaze"]:
                    print(
                        colored("[Status] Reading ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )
                    (
                        time_start_streams_gaze,
                        time_end_streams_gaze,
                    ) = get_start_stop_time_from_xdf(
                        data[i]
                    )  # get the unix time
                    (
                        time_distribution_human_readable_gaze,
                        time_distribution_unix_gaze,
                    ) = create_time_distribution(
                       data[i],
                    )
                    dataframe_to_csv(
                        path,
                        data[i]["time_series"],
                        "Gaze",
                        time_distribution_human_readable_gaze,
                        time_distribution_unix_gaze,
                        rootdir_baseline_task,
                        rootdir_minecraft_data,
                        subject_id,
                        extract_pkl,
                        extract_csv,
                        extract_hdf5,
                        filter,
                        output_path,
                    )

                elif data[i]["info"]["type"] == ["Accelerometer"]:
                    print(
                        colored("[Status] Skipping ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )
                    # create_csv_file(path, 'Accelerometer')
                    # time_start_streams_accel, time_end_streams_accel = get_start_stop_time_from_xdf(data[i]) #get the unix time
        else:
            print(
                colored("[Status] Skipping ", "yellow", attrs=["bold"]),
                colored(exclude, "red"),
            )


def look_for_XDF_files(
    rootdir_xdf,
    rootdir_baseline_task,
    rootdir_minecraft_data,
    subject_id,
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
        rootdir_baseline_task,
        rootdir_minecraft_data,
        subject_id,
        extract_pkl,
        extract_csv,
        extract_hdf5,
        exclude,
        filter,
        output_path,
    )  # 1. read all the XDF files


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Post experiment script for xdf to csv file conversion"
    )
    parser.add_argument(
        "--p1", required=True, help="Path to the directory with the XDF files"
    )

    parser.add_argument(
        "--p2", required=True, help="Enter the Path to folder with baseline task data"
    )

    parser.add_argument(
        "--p3", required=True, help="Enter the Path to folder with minecraft data"
    )

    parser.add_argument(
        "--s",
        required=True,
        action="append",
        help="Enter the Path to folder with baseline task data",
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

    rootdir_xdf = arg.p1
    rootdir_baseline_task = arg.p2
    rootdir_minecraft_data = arg.p3
    subject_id = arg.s
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
            rootdir_baseline_task,
            rootdir_minecraft_data,
            subject_id,
            extract_pkl,
            extract_csv,
            extract_hdf5,
            exclude,
            filter,
            output_path,
        )
    )
