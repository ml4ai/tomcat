import os 
from termcolor import colored
import pandas as pd
from .folder_name_manipulation import get_new_file_paths

def dataframe_to_csv_1(
    path,
    data,
    stream_type,
    time_distribution_human_readable,
    time_distribution_unix,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    output_path,
):
    """
    This is data extraction for experiments prior to 2023_04_17.
    Read data from the XDF file, convert it into a dictionary,
    then convert that to a pandas dataframe and save it as csv file.
    """
    path = os.path.normpath(path + os.sep + os.pardir)

    if stream_type == "NIRS":
        header_raw = [
            "unix_time",
            "human_readable_time",
            "S1-D1_760",
            "S1-D2_760",
            "S2-D1_760",
            "S2-D3_760",
            "S3-D1_760",
            "S3-D3_760",
            "S3-D4_760",
            "S4-D2_760",
            "S4-D4_760",
            "S4-D5_760",
            "S5-D3_760",
            "S5-D4_760",
            "S5-D6_760",
            "S6-D4_760",
            "S6-D6_760",
            "S6-D7_760",
            "S7-D5_760",
            "S7-D7_760",
            "S8-D6_760",
            "S8-D7_760",
            "S1-D1_850",
            "S1-D2_850",
            "S2-D1_850",
            "S2-D3_850",
            "S3-D1_850",
            "S3-D3_850",
            "S3-D4_850",
            "S4-D2_850",
            "S4-D4_850",
            "S4-D5_850",
            "S5-D3_850",
            "S5-D4_850",
            "S5-D6_850",
            "S6-D4_850",
            "S6-D6_850",
            "S6-D7_850",
            "S7-D5_850",
            "S7-D7_850",
            "S8-D6_850",
            "S8-D7_850",
        ]

        header_hbo_hbr = [
            "unix_time",
            "human_readable_time",
            "S1-D1_HbO",
            "S1-D2_HbO",
            "S2-D1_HbO",
            "S2-D3_HbO",
            "S3-D1_HbO",
            "S3-D3_HbO",
            "S3-D4_HbO",
            "S4-D2_HbO",
            "S4-D4_HbO",
            "S4-D5_HbO",
            "S5-D3_HbO",
            "S5-D4_HbO",
            "S5-D6_HbO",
            "S6-D4_HbO",
            "S6-D6_HbO",
            "S6-D7_HbO",
            "S7-D5_HbO",
            "S7-D7_HbO",
            "S8-D6_HbO",
            "S8-D7_HbO",
            "S1-D1_HbR",
            "S1-D2_HbR",
            "S2-D1_HbR",
            "S2-D3_HbR",
            "S3-D1_HbR",
            "S3-D3_HbR",
            "S3-D4_HbR",
            "S4-D2_HbR",
            "S4-D4_HbR",
            "S4-D5_HbR",
            "S5-D3_HbR",
            "S5-D4_HbR",
            "S5-D6_HbR",
            "S6-D4_HbR",
            "S6-D6_HbR",
            "S6-D7_HbR",
            "S7-D5_HbR",
            "S7-D7_HbR",
            "S8-D6_HbR",
            "S8-D7_HbR",
        ]

        header = header_raw + header_hbo_hbr[2:]
        
        channel_list = header[2:]
        index = 1  # This points to index where W1, W2, HbO and HbR channel data starts on the XDF file

    if stream_type == "EEG":
        header = [
            "unix_time",
            "human_readable_time",
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
        channel_list = header[2:]
        index = 0  # EEG starts at the 0th index on the XDF file

    if stream_type == "Gaze":
        header = [
            "unix_time",
            "human_readable_time",
            "confidence",
            "norm_pos_x",
            "norm_pos_y",
            "gaze_point_3d_x",
            "gaze_point_3d_y",
            "gaze_point_3d_z",
            "eye_center0_3d_x",
            "eye_center0_3d_y",
            "eye_center0_3d_z",
            "eye_center1_3d_x",
            "eye_center1_3d_y",
            "eye_center1_3d_z",
            "gaze_normal0_x",
            "gaze_normal0_y",
            "gaze_normal0_z",
            "gaze_normal1_x",
            "gaze_normal1_y",
            "gaze_normal1_z",
            "diameter0_2d",
            "diameter1_2d",
            "diameter0_3d",
            "diameter1_3d",
        ]
        channel_list = header[2:]
        index = 0  # Gaze starts at the 0th index on the XDF file

    data_path = path
    csv_file_name = data_path + "/" + stream_type
    df = pd.DataFrame(columns=header)

    csv_entry = {}
    for i in range(len(data)):
        csv_entry[i] = data[i][index:]

    # 1. Gather all the channel data into the data frame
    df = pd.DataFrame.from_dict(csv_entry, columns=channel_list, orient="index")
    print(
        colored("[INFO]", "green", attrs=["bold"]),
        colored(stream_type, "blue"),
        colored("data written to CSV file", "green", attrs=["bold"]),
    )

    # 2. Gather human readable timestamp distribution
    df[header[1]] = time_distribution_human_readable
    print(
        colored("[INFO]", "green", attrs=["bold"]),
        colored(stream_type, "blue"),
        colored("readable timestamp written to CSV file", "green", attrs=["bold"]),
    )

    # 3. Gather unix timestamp distribution
    df[header[0]] = time_distribution_unix
    print(
        colored("[INFO]", "green", attrs=["bold"]),
        colored(stream_type, "blue"),
        colored("unix timestamp written to CSV file", "green", attrs=["bold"]),
    )

    new_csv_file_path = get_new_file_paths(output_path, csv_file_name)

    # Ensure the directory exists
    if not os.path.exists(os.path.dirname(new_csv_file_path)):
        os.makedirs(os.path.dirname(new_csv_file_path))

    # Save as CSV file
    if extract_csv == True:
        if stream_type == "NIRS":
            print(
                colored("[INFO]", "green", attrs=["bold"]),
                colored(stream_type, "blue"),
                colored("Saving Raw NIRS wavelength info to csv files", "green", attrs=["bold"]),
            )
            df[header_raw].to_csv(new_csv_file_path + "_raw" + ".csv", sep=";", encoding="utf-8")

            print(
                colored("[INFO]", "green", attrs=["bold"]),
                colored(stream_type, "blue"),
                colored("Saving HbO HbR NIRS info to csv files", "green", attrs=["bold"]),
            )

            df[header_hbo_hbr].to_csv(new_csv_file_path + ".csv", sep=";", encoding="utf-8")

        else:
            df.to_csv(new_csv_file_path + ".csv", sep=";", encoding="utf-8")
            print(
                colored("[INFO]", "green", attrs=["bold"]),
                colored("Sucessfully generated csv file at", "green", attrs=["bold"]),
                colored(new_csv_file_path + ".csv", "blue"),
            )

    # Save as pickle file
    if extract_pkl == True:
        df.to_pickle(csv_file_name + ".pkl", sep=";", encoding="utf-8")
        print(
            colored("[INFO]", "green", attrs=["bold"]),
            colored("Sucessfully generated pickle file at", "green", attrs=["bold"]),
            colored(new_csv_file_path + ".pkl", "blue"),
        )

    # Save as hdf5 file
    if extract_hdf5 == True:
        df.to_hdf(new_csv_file_path + ".h5", key="df", mode="w", sep=";", encoding="utf-8")
        print(
            colored("[INFO]", "green", attrs=["bold"]),
            colored("Sucessfully generated hdf5 file at", "green", attrs=["bold"]),
            colored(new_csv_file_path + ".h5", "blue"),
        )
