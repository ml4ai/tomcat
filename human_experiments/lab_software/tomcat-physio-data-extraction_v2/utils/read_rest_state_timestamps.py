from termcolor import colored
import json
import pandas as pd

def read_rest_state_timestamps(block):
    markers = {} 
    idx = 0
    for i in range(0,len(block)):
        if block[i]['info']['name'][0] ==  'RestState':
            rest_state = block[i]['time_stamps']

            print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
            )
            markers[idx] = {"state":"rest_state", 
                            "participant": None,
                       "start_time":rest_state[0],
                       "end_time":rest_state[-1]}

    return markers
