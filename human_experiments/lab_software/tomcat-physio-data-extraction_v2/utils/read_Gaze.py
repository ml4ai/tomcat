import pandas as pd
from termcolor import colored

headers = [
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

def read_gaze(block):
    for i in range(0,len(block)):
        if block[i]['info']['name'][0] == 'pupil_capture' and block[i]['info']['hostname'][0] == 'lion':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            lion_Gaze_block = block[i]
            lion_Gaze_block_df = pd.DataFrame(lion_Gaze_block['time_series'], columns=headers) #Get channel data
                    
            print(colored("[Status] Extracted timeseries from Gaze as dataframe ", "green", attrs=["bold"]))
            lion_Gaze_block_df['unix_time'] = lion_Gaze_block['time_stamps'] #Get Unix time from XDF
            print(colored("[Status] Merged Gaze timestamps with Gaze dataframe ", "green", attrs=["bold"]))

        if block[i]['info']['name'][0] == 'pupil_capture' and block[i]['info']['hostname'][0] == 'tiger':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            tiger_Gaze_block = block[i]
            tiger_Gaze_block_df = pd.DataFrame(tiger_Gaze_block['time_series'], columns=headers)
            
            print(colored("[Status] Extracted timeseries from Gaze as dataframe ", "green", attrs=["bold"]))
            tiger_Gaze_block_df['unix_time'] = tiger_Gaze_block['time_stamps'] #Get Unix time from XDF
            print(colored("[Status] Merged Gaze timestamps with Gaze dataframe ", "green", attrs=["bold"]))
        
        if block[i]['info']['name'][0] == 'pupil_capture' and block[i]['info']['hostname'][0] == 'leopard':
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["type"], "blue"),
                colored(block[i]["info"]["name"], "blue"),
            )
            leopard_Gaze_block = block[i]
            leopard_Gaze_block_df = pd.DataFrame(leopard_Gaze_block['time_series'], columns=headers)
                        
            print(colored("[Status] Extracted timeseries from Gaze as dataframe ", "green", attrs=["bold"]))
            leopard_Gaze_block_df['unix_time'] = leopard_Gaze_block['time_stamps'] #Get Unix time from XDF
            print(colored("[Status] Merged Gaze timestamps with Gaze dataframe ", "green", attrs=["bold"]))

    return lion_Gaze_block_df, tiger_Gaze_block_df, leopard_Gaze_block_df