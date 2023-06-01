import json
import pandas as pd
from termcolor import colored

def read_finger_tapping_time(block):
        for i in range(0,len(block)):
            if block[i]['info']['name'][0] ==  'FingerTapping':
                
                print(colored("[Status] Reading ", "green", attrs=["bold"]),
                colored(block[i]["info"]["name"], "blue"),
                )
                
                finger_tapping = block[i]['time_series']
                finger_tapping_strings = [item[0] for item in finger_tapping]

                # Parse JSON strings into dictionaries
                data = [json.loads(d) for d in finger_tapping_strings]

                # Convert list of dictionaries to DataFrame
                df = pd.DataFrame(data)

        return df
