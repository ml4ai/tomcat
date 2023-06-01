import json
import pandas as pd
from termcolor import colored

def read_affective_task_timestamps_individual(block):
    for i in range(0,len(block)):
        if block[i]['info']['name'][0] == 'AffectiveTask_tiger':

            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
                )
            
            # print(block[i]['time_series'])
            AffectiveTask_tiger = block[i]['time_series']
            finger_tapping_strings = [item[0] for item in AffectiveTask_tiger]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in finger_tapping_strings]

            # Convert list of dictionaries to DataFrame
            AffectiveTask_tiger = pd.DataFrame(data)

        if block[i]['info']['name'][0] == 'AffectiveTask_lion':

            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
                )
            
            # print(block[i]['time_series'])
            AffectiveTask_lion = block[i]['time_series']
            finger_tapping_strings = [item[0] for item in AffectiveTask_lion]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in finger_tapping_strings]

            # Convert list of dictionaries to DataFrame
            AffectiveTask_lion = pd.DataFrame(data)

        if block[i]['info']['name'][0] == 'AffectiveTask_leopard':
                
                print(colored("[Status] Reading ", "green", attrs=["bold"]),
                    colored(block[i]["info"]["name"], "blue"),
                    )
                
                # print(block[i]['time_series'])
                AffectiveTask_leopard = block[i]['time_series']
                AffectiveTask_leopard_strings = [item[0] for item in AffectiveTask_leopard]
    
                # Parse JSON strings into dictionaries
                data = [json.loads(d) for d in AffectiveTask_leopard_strings]
    
                # Convert list of dictionaries to DataFrame
                AffectiveTask_leopard = pd.DataFrame(data)

    return AffectiveTask_tiger, AffectiveTask_lion, AffectiveTask_leopard

def read_affective_task_timestamps_team(block):
    for i in range(0,len(block)):
        if block[i]['info']['name'][0] == 'AffectiveTask_team':

            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
                )
            
            # print(block_1[i]['time_series'])
            AffectiveTask_team= block[i]['time_series']
            AffectiveTask_team_strings = [item[0] for item in AffectiveTask_team]

            # Parse JSON strings into dictionaries
            data = [json.loads(d) for d in AffectiveTask_team_strings]

            # Convert list of dictionaries to DataFrame
            AffectiveTask_team = pd.DataFrame(data)

            return AffectiveTask_team
