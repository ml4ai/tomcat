import os
import numpy as np
import pandas as pd
from termcolor import colored

def save_NIRS(
    lion_0297_block_NIRS,
    tiger_0239_block_NIRS,
    leopard_0171_block_NIRS,
    lion_0297_raw,
    tiger_0239_raw,
    leopard_0171_raw,
    input_path,
    output_path,
    extract_pkl,
    extract_csv,
    extract_hdf5,
):
    # Construct a dictionary to loop through
    df_dict = {
        "lion": [lion_0297_block_NIRS, lion_0297_raw],
        "tiger": [tiger_0239_block_NIRS, tiger_0239_raw],
        "leopard": [leopard_0171_block_NIRS, leopard_0171_raw],
    }

    # Extract the folder name from the input path
    print("Input path: {}".format(input_path))
    folder_name = os.path.basename(input_path)

    for iMac, df in df_dict.items():
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
                colored("Saving unfiltered NIRS to", "green", attrs=["bold"]),
                colored(file_path, "green", attrs=["bold"]),
            )
            df[0].to_csv(file_path)
            df[1].to_csv(file_path_raw)

        if extract_pkl:
            # Create the full file path
            file_path = os.path.join(full_output_path, "NIRS.pkl")

            # Save the dataframe to a pkl file
            print("Saving NIRS data to: {}".format(file_path))
            print(
                colored("[INFO]", "green", attrs=["bold"]),
                colored("Saving unfiltered NIRS to", "green", attrs=["bold"]),
                colored(file_path, "green", attrs=["bold"]),
            )
            df[0].to_pickle(file_path)

        if extract_hdf5:
            # Create the full file path
            file_path = os.path.join(full_output_path, "NIRS.h5")

            # Save the dataframe to a hdf5 file
            print("Saving NIRS data to: {}".format(file_path))
            print(
                colored("[INFO]", "green", attrs=["bold"]),
                colored("Saving unfiltered NIRS to", "green", attrs=["bold"]),
                colored(file_path, "green", attrs=["bold"]),
            )
            df[0].to_hdf(file_path, key="df", mode="w")
