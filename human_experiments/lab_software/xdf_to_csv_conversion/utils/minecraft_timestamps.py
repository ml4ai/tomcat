import os
import json
import argparse
import datetime
import pytz
from time import ctime
from termcolor import colored 
from .convert_datetime_timezone import change_time_zone

def read_minecraft_time(baseline_task_dict, rootdir_minecraft_data):
    """
    Minecraft doesn't have unix time, hence we convert ISO-8601 
    format to unix time. Minecraft uses UTC timezone so we have 
    convert it to MST timezone (done by change_time_zone function), 
    since that is how baseline and xdf logs timestamps. Also every 
    trial has a common start and stop timestamps. 
    """

    minecraft_dict = baseline_task_dict
    idx_minecraft_dict = len(minecraft_dict)
    for x in os.listdir(rootdir_minecraft_data):
            if x.endswith(".metadata"):
                root_dir = os.path.join(rootdir_minecraft_data, x)
                TrialMessages = []
                with open(root_dir, "r") as f:
                    for line in f:
                        try:
                            json_message = json.loads(line)
                            TrialMessages.append(json_message)
                            if (
                                json_message["header"]["message_type"] == "event"
                                and json_message["msg"]["sub_type"] == "Event:MissionState"
                            ):
                                if json_message["data"]["mission_state"] == "Start":
                                    trial_start = json_message["msg"]["timestamp"]
                                else:
                                    trial_end = json_message["msg"]["timestamp"]
                        except:
                            print(
                                colored("[Error] Cannot read JSON line", "red"),
                                "\N{cross mark}",
                            )
                    
                    for i in range(len(TrialMessages)):
                        try:
                            # testbed_version = TrialMessages[i]['data']['testbed_version']
                            map_name = TrialMessages[i]['data']['map_name']
                            for idx, sub in enumerate(TrialMessages[i]['data']['subjects']):
                                subject_id = sub
                                call_sign = TrialMessages[i]['data']['client_info'][idx]['callsign']
                                playername = TrialMessages[i]['data']['client_info'][idx]['playername']
                            if idx == 2:
                                    break
                        except:
                            continue

                    trial_start = ( trial_start.split("T")[0] + " " + trial_start.split("T")[1].split("Z")[0])   
                    trial_start = change_time_zone(trial_start, "UTC", "MST")        
                    trial_start = datetime.datetime.strptime(str(trial_start), "%Y-%m-%d %H:%M:%S.%f")
                    trial_start = trial_start.timestamp()

                    trial_end = ( trial_end.split("T")[0] + " " + trial_end.split("T")[1].split("Z")[0])     
                    trial_end = change_time_zone(trial_end, "UTC", "MST")     
                    trial_end = datetime.datetime.strptime(str(trial_end), "%Y-%m-%d %H:%M:%S.%f")
                    trial_end = trial_end.timestamp()

                    # print(ctime(trial_start), ctime(trial_end), trial_start, trial_end, map_name, subject_id, call_sign, playername)
                    
                    minecraft_dict[idx_minecraft_dict] = {'state':map_name, 'participant': None, 
                                            'start_time': trial_start, 
                                            'end_time':trial_end}
                    # print(idx_minecraft_dict)
                    idx_minecraft_dict += 1

    # print(minecraft_dict)

# read_minecraft_time('a', '/Users/calebjonesshibu/Desktop/tom/dry_runs/exp_2022_09_13_10/exp_2022_09_13_10/minecraft/')
