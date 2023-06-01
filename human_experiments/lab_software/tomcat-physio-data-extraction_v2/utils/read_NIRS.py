import pandas as pd
from termcolor import colored

headers = [
    "S1-D1_HbO", "S1-D2_HbO", "S2-D1_HbO", "S2-D3_HbO", "S3-D1_HbO",
    "S3-D3_HbO", "S3-D4_HbO", "S4-D2_HbO", "S4-D4_HbO", "S4-D5_HbO",
    "S5-D3_HbO", "S5-D4_HbO", "S5-D6_HbO", "S6-D4_HbO", "S6-D6_HbO",
    "S6-D7_HbO", "S7-D5_HbO", "S7-D7_HbO", "S8-D6_HbO", "S8-D7_HbO",
    "S1-D1_HbR", "S1-D2_HbR", "S2-D1_HbR", "S2-D3_HbR", "S3-D1_HbR",
    "S3-D3_HbR", "S3-D4_HbR", "S4-D2_HbR", "S4-D4_HbR", "S4-D5_HbR",
    "S5-D3_HbR", "S5-D4_HbR", "S5-D6_HbR", "S6-D4_HbR", "S6-D6_HbR",
    "S6-D7_HbR", "S7-D5_HbR", "S7-D7_HbR", "S8-D6_HbR", "S8-D7_HbR"
]

def read_nirs(block):
    for i in range(0,len(block)):
        if block[i]['info']['name'][0] == 'lion_0297' and block[i]['info']['type'][0] == 'NIRS':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            lion_0297_block = block[i]
            lion_0297_block_df = pd.DataFrame(lion_0297_block['time_series'][:, 41:], columns=headers) #Get channel data
            print(colored("[Status] Extracted timeseries from NIRS as dataframe ", "green", attrs=["bold"]))
            lion_0297_block_df['unix_time'] = lion_0297_block['time_stamps'] #Get Unix time
            print(colored("[Status] Merged NIRS timestamps with NIRS dataframe ", "green", attrs=["bold"]))

        if block[i]['info']['name'][0] == 'tiger_0239' and block[i]['info']['type'][0] == 'NIRS':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            tiger_0239_block = block[i]
            tiger_0239_block_df = pd.DataFrame(tiger_0239_block['time_series'][:, 41:], columns=headers)
            print(colored("[Status] Extracted timeseries from NIRS as dataframe ", "green", attrs=["bold"]))
            tiger_0239_block_df['unix_time'] = tiger_0239_block['time_stamps']
            print(colored("[Status] Merged NIRS timestamps with NIRS dataframe ", "green", attrs=["bold"]))
        
        if block[i]['info']['name'][0] == 'leopard_0171' and block[i]['info']['type'][0] == 'NIRS':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            leopard_0171_block = block[i]
            leopard_0171_block_df = pd.DataFrame(leopard_0171_block['time_series'][:, 41:], columns=headers)
            print(colored("[Status] Extracted timeseries from NIRS as dataframe ", "green", attrs=["bold"]))
            leopard_0171_block_df['unix_time'] = leopard_0171_block['time_stamps']
            print(colored("[Status] Merged NIRS timestamps with NIRS dataframe ", "green", attrs=["bold"]))

    return lion_0297_block, tiger_0239_block, leopard_0171_block