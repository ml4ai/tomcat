import json
import pandas as pd
from termcolor import colored

def read_ping_pong_timestamps(block):
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

    return PingPong_competitive_0, PingPong_competitive_1, PingPong_cooperative_0
            