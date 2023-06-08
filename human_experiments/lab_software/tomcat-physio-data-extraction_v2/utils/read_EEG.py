import pandas as pd
from termcolor import colored

headers = [
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

def read_eeg(block):
    # Initialize empty dataframes
    lion_EEG_block_df = pd.DataFrame()
    tiger_EEG_block_df = pd.DataFrame()
    leopard_EEG_block_df = pd.DataFrame()
   
    for i in range(0,len(block)):
        try:
            if block[i]['info']['name'][0] == 'actiCHamp-21030529' and block[i]['info']['type'][0] == 'EEG':
                print(colored("[Info] Reading ", "green", attrs=["bold"]),
                    colored(block[i]["info"]["type"], "blue"),
                    colored(block[i]["info"]["name"], "blue"),
                )
                lion_EEG_block = block[i]
                lion_EEG_block_df = pd.DataFrame(lion_EEG_block['time_series'], columns=headers) #Get channel data
                
                print(colored("[Status] Extracted timeseries from EEG as dataframe ", "green", attrs=["bold"]))
                lion_EEG_block_df['unix_time'] = lion_EEG_block['time_stamps'] #Get Unix time from XDF
                print(colored("[Status] Merged EEG timestamps with EEG dataframe ", "green", attrs=["bold"]))

            if block[i]['info']['name'][0] == 'actiCHamp-20010205' and block[i]['info']['type'][0] == 'EEG':
                print(colored("[Info] Reading ", "green", attrs=["bold"]),
                    colored(block[i]["info"]["type"], "blue"),
                    colored(block[i]["info"]["name"], "blue"),
                )
                tiger_EEG_block = block[i]
                tiger_EEG_block_df = pd.DataFrame(tiger_EEG_block['time_series'], columns=headers)
                
                print(colored("[Status] Extracted timeseries from EEG as dataframe ", "green", attrs=["bold"]))
                tiger_EEG_block_df['unix_time'] = tiger_EEG_block['time_stamps'] #Get Unix time from XDF
                print(colored("[Status] Merged EEG timestamps with EEG dataframe ", "green", attrs=["bold"]))
            
            if block[i]['info']['name'][0] == 'actiCHamp-21020492' and block[i]['info']['type'][0] == 'EEG':
                print(colored("[Info] Reading ", "green", attrs=["bold"]),
                    colored(block[i]["info"]["type"], "blue"),
                    colored(block[i]["info"]["name"], "blue"),
                )
                leopard_EEG_block = block[i]
                leopard_EEG_block_df = pd.DataFrame(leopard_EEG_block['time_series'], columns=headers)
                
                print(colored("[Status] Extracted timeseries from EEG as dataframe ", "green", attrs=["bold"]))
                leopard_EEG_block_df['unix_time'] = leopard_EEG_block['time_stamps'] #Get Unix time from XDF
                print(colored("[Status] Merged EEG timestamps with EEG dataframe ", "green", attrs=["bold"]))
        except:
            print(colored("[Error] EEG data not found in XDF file", "red", attrs=["bold"]), 
                colored(block[i]['info']['name'][0], "blue",attrs=["bold"]))
            continue

    return lion_EEG_block_df, tiger_EEG_block_df, leopard_EEG_block_df