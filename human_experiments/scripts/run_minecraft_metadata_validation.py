import sys 
import os
import json
import argparse
import datetime
import pandas as pd
from termcolor import colored
from time import sleep

def check_time_difference(trial_start, trial_end):
    #check difference in time
    sleep(0.25)
    trial_start = trial_start.split('T')[0] + " " + trial_start.split('T')[1].split('Z')[0]
    trial_end = trial_end.split('T')[0] + " " + trial_end.split('T')[1].split('Z')[0]

    print(colored('\n[Status] Mission started at time:', 'red', attrs=['bold']), trial_start)
    print(colored('\n[Status] Mission ended at time:', 'red', attrs=['bold']), trial_end)

    trial_start = datetime.datetime.strptime(str(trial_start), '%Y-%m-%d %H:%M:%S.%f')
    trial_start = trial_start.timestamp()
    trial_end = datetime.datetime.strptime(str(trial_end), '%Y-%m-%d %H:%M:%S.%f')
    trial_end = trial_end.timestamp()
    delta = int(float(trial_end) - float(trial_start))
    min, sec = divmod(delta, 60)

    if min > 0:
        print(colored('\n[Status] Trial lasted for: ', 'red', attrs=['bold']), min, 'Minutes', sec, 'Seconds')
    else:
        print(colored('[Error] Timing for mission is off by a large margin','red'), u'\N{cross mark}')

def read_subject_id(TrialMessages):
    #Display subject IDs
    sleep(0.25)
    print(colored('\n[Status] Subject info:', 'red', attrs=['bold']))
    for idx, sub in enumerate(TrialMessages[0]['data']['metadata']['trial']['subjects']):
        print(colored('\t Subect ID','magenta'), idx,":", sub)

    #Display trial name
    sleep(0.25)
    print(colored('\n[Status] Mission name:', 'red', attrs=['bold']),TrialMessages[0]['data']['metadata']['trial']['name'])

def read_metadata_as_json(path):
    TrialMessages = []
    with open(path, 'r') as f:
        for line in f:
            try:  
                json_message = json.loads(line)
                TrialMessages.append(json_message)
                if json_message["header"]["message_type"] == "event" and \
                    json_message["msg"]["sub_type"] == "Event:MissionState":
                    if json_message["data"]["mission_state"] == "Start":
                        mission_start = json_message["msg"]["timestamp"]
                    else:
                        mission_end = json_message["msg"]["timestamp"]
            except:
                print("[Error] Cannot read json line")
    read_subject_id(TrialMessages)
    check_time_difference(mission_start, mission_end)

def checkfile(rootdir):
    count = 0
    dir = os.listdir(rootdir)

    if len(dir) == 0:
        #sometime .DS_Store might be there so the length would be 1
        print(colored('\n[Error] Metadata file is missing','red', attrs=['bold']), u'\N{cross mark}')
    else:
        for x in os.listdir(rootdir):
            if x.endswith('.metadata'):
                #check if .metadata file exists or not
                sleep(0.25)
                print(colored('\n[Status] Metadata File:', 'red', attrs=['bold']), colored(os.path.join(rootdir,x), 'green'), u'\N{check mark}')

                if os.stat(os.path.join(rootdir,x)).st_size == 0:
                    #check if .metadata file is empty or not
                    print(colored('\n[Error] Metadata file is empty','red', attrs=['bold']), u'\N{cross mark}')
                else:
                    read_metadata_as_json(os.path.join(rootdir,x))
            elif x.endswith('.DS_Store'):
                #sometime .DS_Store might be there
                continue
            else:
                print(colored('\n[Error] Metadata file is missing','red', attrs=['bold']), u'\N{cross mark}')

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Minecraft Metadata Data Validation')
    parser.add_argument("--p", required=True, help="Enter the Path to folder with baseline task data")
    arg = parser.parse_args()
    rootdir = arg.p
    print(colored('[Status] Root Directory:', 'red', attrs=['bold']), colored(rootdir, 'blue'))
    sys.exit(checkfile(rootdir))

