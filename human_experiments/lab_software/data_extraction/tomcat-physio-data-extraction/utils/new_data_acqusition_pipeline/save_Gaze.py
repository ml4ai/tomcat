import os
import pandas as pd
from termcolor import colored

def save_Gaze(
    lion_0297_block_Gaze_labeled,
    tiger_0239_block_Gaze_labeled,
    leopard_0171_block_Gaze_labeled,
    input_path,
    output_path,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    exclude,
):
    # Construct a dictionary to loop through
    df_dict = {
        "lion": lion_0297_block_Gaze_labeled,
        "tiger": tiger_0239_block_Gaze_labeled,
        "leopard": leopard_0171_block_Gaze_labeled,
    }

    # Extract the folder name from the input path
    print("Input path: {}".format(input_path))
    folder_name = os.path.basename(input_path)

    for iMac, df in df_dict.items():
        if exclude not in iMac:
            if not df.empty:
                # Create the full output path
                full_output_path = os.path.join(output_path, folder_name, iMac)

                # Ensure the directory exists
                os.makedirs(full_output_path, exist_ok=True)

                if extract_csv:
                    # Create the full file path
                    file_path = os.path.join(full_output_path, "Gaze.csv")

                    # Save the dataframe to a csv file
                    print(
                        colored("[INFO]", "green", attrs=["bold"]),
                        colored("Saving unfiltered Gaze to", "green", attrs=["bold"]),
                        colored(file_path, "green", attrs=["bold"]),
                    )
                    df.to_csv(file_path, sep=";", encoding="utf-8")

                if extract_pkl:
                    # Create the full file path
                    file_path = os.path.join(full_output_path, "Gaze.pkl")

                    # Save the dataframe to a pkl file
                    print("Saving Gaze data to: {}".format(file_path))
                    print(
                        colored("[INFO]", "green", attrs=["bold"]),
                        colored("Saving unfiltered Gaze to", "green", attrs=["bold"]),
                        colored(file_path, "green", attrs=["bold"]),
                    )
                    df.to_pickle(file_path, sep=";", encoding="utf-8")

                if extract_hdf5:
                    # Create the full file path
                    file_path = os.path.join(full_output_path, "Gaze.h5")

                    # Save the dataframe to a hdf5 file
                    print("Saving Gaze data to: {}".format(file_path))
                    print(
                        colored("[INFO]", "green", attrs=["bold"]),
                        colored("Saving unfiltered Gaze to", "green", attrs=["bold"]),
                        colored(file_path, "green", attrs=["bold"]),
                    )
                    df.to_hdf(file_path, key="df", mode="w", sep=";", encoding="utf-8")
            else:
                print(
                    colored("[Warning]", "yellow", attrs=["bold"]),
                    colored("No Gaze data to save for", "yellow", attrs=["bold"]),
                    colored(iMac, "green", attrs=["bold"]),
                )
        else:
            print(
                colored("[Warning]", "yellow", attrs=["bold"]),
                colored("Excluding", "yellow", attrs=["bold"]),
                colored(iMac, "green", attrs=["bold"]),
            )