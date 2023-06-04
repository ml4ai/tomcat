import json
import pandas as pd
from termcolor import colored

def read_ping_pong_timestamps(block, AffectiveTask_team_marker):
    idx = len(AffectiveTask_team_marker)
    markers = AffectiveTask_team_marker

    for i in range(0,len(block)):
        if block[i]['info']['name'][0] == 'PingPong_competitive_0':
            #Lion and Tiger compete against each other.
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
                )

            # print(block[i]['time_series'])
            PingPong_competitive_0= block[i]['time_series']
            PingPong_competitive_0_strings = [item[0] for item in PingPong_competitive_0]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in PingPong_competitive_0_strings]

            # Convert list of dictionaries to DataFrame
            PingPong_competitive_0 = pd.DataFrame(data)
            PingPong_competitive_0['lsl_timestamp']  =  block[i]['time_stamps']
            
            markers[idx] = {"state":"ping_pong_competetive_0", 
                            "participant":{'lion', 'tiger'},
                       "start_time":PingPong_competitive_0['lsl_timestamp'].iloc[0],
                       "end_time":PingPong_competitive_0['lsl_timestamp'].iloc[-1]}
            idx += 1

        if block[i]['info']['name'][0] == 'PingPong_competitive_1':
            #Leopard and experimentor compete with each other. 
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
                )

            # print(block[i]['time_series'])
            PingPong_competitive_1= block[i]['time_series']
            PingPong_competitive_1_strings = [item[0] for item in PingPong_competitive_1]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in PingPong_competitive_1_strings]

            # Convert list of dictionaries to DataFrame
            PingPong_competitive_1 = pd.DataFrame(data)
            PingPong_competitive_1['lsl_timestamp']  =  block[i]['time_stamps']

            markers[idx] = {"state":"ping_pong_competetive_1",
                            "participant":{'leopard'},
                          "start_time":PingPong_competitive_1['lsl_timestamp'].iloc[0],
                            "end_time":PingPong_competitive_1['lsl_timestamp'].iloc[-1]}
            idx += 1

        if block[i]['info']['name'][0] == 'PingPong_cooperative_0':
            #Lion, Tiger and Leopard cooperate and compete againsts an AI. 
            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
                )

            # print(block_1[i]['time_series'])
            PingPong_cooperative_0= block[i]['time_series']
            PingPong_cooperative_0_strings = [item[0] for item in PingPong_cooperative_0]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in PingPong_cooperative_0_strings]

            # Convert list of dictionaries to DataFrame
            PingPong_cooperative_0 = pd.DataFrame(data)
            PingPong_cooperative_0['lsl_timestamp']  =  block[i]['time_stamps']

            markers[idx] = {"state":"ping_pong_cooperative_0",
                            "participant":None,
                            "start_time":PingPong_cooperative_0['lsl_timestamp'].iloc[0],
                            "end_time":PingPong_cooperative_0['lsl_timestamp'].iloc[-1]}
            idx += 1

    return markers
            