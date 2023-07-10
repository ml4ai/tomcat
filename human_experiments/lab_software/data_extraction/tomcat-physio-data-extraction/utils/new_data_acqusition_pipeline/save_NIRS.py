import os
import numpy as np
import pandas as pd
from termcolor import colored

def save_NIRS(
    lion_0297_block_NIRS,
    tiger_0239_block_NIRS,
    leopard_0171_block_NIRS,
    input_path,
    output_path,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    exclude,
):
    # define header for NIRS raw and HbO HbR data
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
    # Construct a dictionary to loop through
    df_dict = {
        "lion": lion_0297_block_NIRS,
        "tiger": tiger_0239_block_NIRS,
        "leopard": leopard_0171_block_NIRS,
    }

    # Extract the folder name from the input path
    print("Input path: {}".format(input_path))
    folder_name = os.path.basename(input_path)

    for iMac, df in df_dict.items():
        if exclude not in iMac:
            if not df.empty:
                # Sometimes the dataframe is empty because NIRS wasn't recorded

                # Create the full output path
                full_output_path = os.path.join(output_path, folder_name, iMac)

                # Ensure the directory exists
                os.makedirs(full_output_path, exist_ok=True)

                if extract_csv:
                    # Create the full file path
                    file_path = os.path.join(full_output_path, "NIRS.csv")
                    file_path_raw = os.path.join(full_output_path, "NIRS_raw.csv")

                    # Save the dataframe to a csv file
                    print(
                        colored("[INFO]", "green", attrs=["bold"]),
                        colored("Saving unfiltered NIRS HbO HbR data to", "green", attrs=["bold"]),
                        colored(file_path, "green", attrs=["bold"]),
                    )
                    df[header_hbo_hbr].to_csv(file_path, sep=";", encoding="utf-8")

                    print(
                        colored("[INFO]", "green", attrs=["bold"]),
                        colored("Saving raw NIRS W1 W2 to", "green", attrs=["bold"]),
                        colored(file_path, "green", attrs=["bold"]),
                    )
                    df[header_raw].to_csv(file_path_raw, sep=";", encoding="utf-8")

                if extract_pkl:
                    # Create the full file path
                    file_path = os.path.join(full_output_path, "NIRS.pkl")
                    file_path_raw = os.path.join(full_output_path, "NIRS_raw.pkl")

                    # Save the dataframe to a pkl file
                    print("Saving NIRS data to: {}".format(file_path))
                    print(
                        colored("[INFO]", "green", attrs=["bold"]),
                        colored("Saving unfiltered NIRS HbO HbR data to", "green", attrs=["bold"]),
                        colored(file_path, "green", attrs=["bold"]),
                    )
                    df[header_hbo_hbr].to_pickle(file_path, sep=";", encoding="utf-8")
                    df[header_raw].to_pickle(file_path_raw, sep=";", encoding="utf-8")

                if extract_hdf5:
                    # Create the full file path
                    file_path = os.path.join(full_output_path, "NIRS.h5")
                    file_path_raw = os.path.join(full_output_path, "NIRS_raw.h5")

                    # Save the dataframe to a hdf5 file
                    print("Saving NIRS data to: {}".format(file_path))
                    print(
                        colored("[INFO]", "green", attrs=["bold"]),
                        colored("Saving unfiltered NIRS HbO HbR data to", "green", attrs=["bold"]),
                        colored(file_path, "green", attrs=["bold"]),
                    )
                    df[header_hbo_hbr].to_hdf(file_path, key="df", mode="w", sep=";", encoding="utf-8")
                    df[header_raw].to_hdf(file_path_raw, key="df", mode="w", sep=";", encoding="utf-8")
            else:
                print(
                    colored("[Warning]", "yellow", attrs=["bold"]),
                    colored("No NIRS data to save for", "yellow", attrs=["bold"]),
                    colored(iMac, "green", attrs=["bold"]),
                )
        else:
            print(
                colored("[Warning]", "yellow", attrs=["bold"]),
                colored("Excluding", "yellow", attrs=["bold"]),
                colored(iMac, "green", attrs=["bold"]),
            )