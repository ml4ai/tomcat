import os
import mne 
import numpy as np
import pandas as pd
from termcolor import colored

def filter_EEG(data, full_output_path):
    print(
         colored("[INFO]", "green", attrs=["bold"]),
         colored("Filtering EEG data", "green", attrs=["bold"]),
     )
    sfreq = 500
    
    '''
    You need to convert microvolts to volts by multiplying by 1e-6, and it applies a preamp gain adjustment by dividing by 50 and then halving the result. If we only want to perform the conversion from microvolts to volts, the operation simplifies to data *= 1e-6.
    '''

    #1. Get EKG data
    EKG_data = data['AUX_EKG'] * 1e-6 
    info_EKG = mne.create_info(['AUX_EKG'], sfreq, 'ecg')
    raw_EKG = mne.io.RawArray(np.array(EKG_data)[np.newaxis, :], info_EKG)

    #2. Get GSR data
    GSR_data = data['AUX_GSR'] * 1e-6
    info_GSR = mne.create_info(['AUX_GSR'], sfreq, 'misc')
    raw_GSR = mne.io.RawArray(np.array(GSR_data)[np.newaxis, :], info_GSR)

    #3. Get EEG data
    channels_used = [
                        "AFF1h", "F7", "FC5", "C3", "T7", "TP9", "Pz", "P3", "P7", "O1", "O2", "P8",
                        "P4", "TP10", "Cz", "C4", "T8", "FC6", "FCz", "F8", "AFF2h"
                    ]
    EEG_data = data[channels_used] * 1e-6
    info_EEG = mne.create_info(channels_used, sfreq, 'eeg')
    raw_EEG = mne.io.RawArray(np.array(EEG_data).T, info_EEG)
    EEG_montage = 'standard_1005' # Electrode position file
    raw_EEG.set_montage(EEG_montage)
    raw_EEG_filtered = raw_EEG.copy().notch_filter(freqs=60, filter_length='auto', trans_bandwidth=9, notch_widths=2)

    #4. Replace original data with filtered data
    data['AUX_EKG'] = raw_EKG.get_data()[0]
    data['AUX_GSR'] = raw_GSR.get_data()[0]
    data[channels_used] = raw_EEG_filtered.get_data().T

    file_path = os.path.join(full_output_path, "EEG_filtered.csv")
    print(
         colored("[STATUS]", "green", attrs=["bold"]),
         colored("Saving filtered EEG data to", "green", attrs=["bold"]),
         colored(file_path, "green", attrs=["bold"]),
     )
    data.to_csv(file_path, index=False)

def save_EEG(lion_0297_block_EEG_labeled, tiger_0239_block_EEG_labeled, leopard_0171_block_EEG_labeled, input_path, output_path, extract_pkl, extract_csv, extract_hdf5, filter):
    # Construct a dictionary to loop through
    df_dict = {"lion": lion_0297_block_EEG_labeled, "tiger": tiger_0239_block_EEG_labeled, "leopard": leopard_0171_block_EEG_labeled}
    
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
            file_path = os.path.join(full_output_path, "EEG.csv")

            # Save the dataframe to a csv file
            print(
            colored("[INFO]", "green", attrs=["bold"]),
            colored("Saving unfiltered EEG to", "green", attrs=["bold"]),
            colored(file_path, "green", attrs=["bold"]),
            )
            df.to_csv(file_path)

        if extract_pkl:
            # Create the full file path
            file_path = os.path.join(full_output_path, "EEG.pkl")
            
            # Save the dataframe to a pkl file
            print("Saving EEG data to: {}".format(file_path))
            print(
            colored("[INFO]", "green", attrs=["bold"]),
            colored("Saving unfiltered EEG to", "green", attrs=["bold"]),
            colored(file_path, "green", attrs=["bold"]),
            )
            df.to_pickle(file_path)
        
        if extract_hdf5:
            # Create the full file path
            file_path = os.path.join(full_output_path, "EEG.h5")
            
            # Save the dataframe to a hdf5 file
            print("Saving EEG data to: {}".format(file_path))
            print(
            colored("[INFO]", "green", attrs=["bold"]),
            colored("Saving unfiltered EEG to", "green", attrs=["bold"]),
            colored(file_path, "green", attrs=["bold"]),
            )
            df[0].to_hdf(file_path, key="df", mode="w")
    
        if filter:
            filter_EEG(df, full_output_path)