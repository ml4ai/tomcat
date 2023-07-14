import shutil
import numpy as np
import pyxdf
from termcolor import colored
from utils import (
    create_time_distribution,
    dataframe_to_csv_1,
)

def read_xdf(
    xdf_file_paths,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    exclude,
    output_path,
):
    """
    Read the XDF files for experiments including 2023_04_17 and after.
    """
    columns = shutil.get_terminal_size().columns
    for path in xdf_file_paths:
        data, _ = pyxdf.load_xdf(path)
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
                        time_distribution_human_readable_nirs,
                        time_distribution_unix_nirs,
                    ) = create_time_distribution(
                        data[i],
                    )
                    dataframe_to_csv_1(
                        path,
                        data[i]["time_series"],
                        "NIRS",
                        time_distribution_human_readable_nirs,
                        time_distribution_unix_nirs,
                        extract_pkl,
                        extract_csv,
                        extract_hdf5,
                        output_path,
                    )

                elif data[i]["info"]["type"] == ["Markers"]:
                    # Our experiments don't use physical marker for the physio data
                    print(
                        colored("[Status] Skipping ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )

                elif data[i]["info"]["type"] == ["EEG"]:
                    print(
                        colored("[Status] Reading ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )
                    (
                        time_distribution_human_readable_eeg,
                        time_distribution_unix_eeg,
                    ) = create_time_distribution(data[i])

                    # Create a list of channel names
                    EEG_channels = []
                    for channel_dict in data[i]["info"]["desc"][0]["channels"][0][
                        "channel"
                    ]:
                        EEG_channels.append(channel_dict["label"][0])

                    channels_used = [
                        "AFF1h",
                        "F7",
                        "FC5",
                        "C3",
                        "T7",
                        "TP9",
                        "Pz",
                        "P3",
                        "P7",
                        "O1",
                        "O2",
                        "P8",
                        "P4",
                        "TP10",
                        "Cz",
                        "C4",
                        "T8",
                        "FC6",
                        "FCz",
                        "F8",
                        "AFF2h",
                        "AUX_GSR",
                        "AUX_EKG",
                    ]

                    exclude_channels = [
                        (i, ch)
                        for i, ch in enumerate(EEG_channels)
                        if ch not in channels_used
                    ]
                    exclude_indices = [index for index, _ in exclude_channels]
                    EEG_data = np.delete(
                        data[i]["time_series"].T, exclude_indices, axis=0
                    )

                    dataframe_to_csv_1(
                        path,
                        EEG_data.T,  # Use EEG data with channels we want
                        "EEG",
                        time_distribution_human_readable_eeg,
                        time_distribution_unix_eeg,
                        extract_pkl,
                        extract_csv,
                        extract_hdf5,
                        output_path,
                    )

                elif data[i]["info"]["type"] == ["Gaze"]:
                    print(
                        colored("[Status] Reading ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )
                    (
                        time_distribution_human_readable_gaze,
                        time_distribution_unix_gaze,
                    ) = create_time_distribution(
                        data[i],
                    )
                    dataframe_to_csv_1(
                        path,
                        data[i]["time_series"],
                        "Gaze",
                        time_distribution_human_readable_gaze,
                        time_distribution_unix_gaze,
                        extract_pkl,
                        extract_csv,
                        extract_hdf5,
                        output_path,
                    )

                elif data[i]["info"]["type"] == ["Accelerometer"]:
                    print(
                        colored("[Status] Skipping ", "green", attrs=["bold"]),
                        colored(data[i]["info"]["type"], "blue"),
                    )
        else:
            print(
                colored("[Warning]", "yellow", attrs=["bold"]),
                colored("Excluding", "yellow", attrs=["bold"]),
                colored(exclude, "green", attrs=["bold"]),
            )
            