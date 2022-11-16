import os
import json
import datetime
import numpy as np
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
    minecraft_dict_temp = {'state':[], 
                        'start_time': [], 
                        'end_time' : []}
    minecraft_dict = baseline_task_dict
    idx_minecraft_dict = len(minecraft_dict)
    for x in os.listdir(rootdir_minecraft_data):
            if x.endswith(".metadata"):
                root_dir = os.path.join(rootdir_minecraft_data, x)
                TrialMessages = []
                with open(root_dir, "r") as f:
                    condition = []
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

                                    condition.append('Start')
                                if json_message["data"]["mission_state"] == "Stop":
                                    trial_end = json_message["msg"]["timestamp"]

                                    condition.append('Stop')
                                    
                            if len(condition) == 2:
                                continue
                            else: 
                                if (
                                    json_message["header"]["message_type"] == "trial"
                                    ):
                                    if json_message["msg"]["sub_type"] == "stop":
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
                    minecraft_dict_temp['state'].append(map_name)
                    minecraft_dict_temp['start_time'].append(trial_start)
                    minecraft_dict_temp['end_time'].append(trial_end)

    """
    We need a logic to differentiate between 
    saturn a and saturn b minecraft missions as
    they both have map name Saturn_2.6_3D. We do 
    this by sorting the dictionary by the missions
    start_time and hard coding map name based on 
    sorted index. 
    """                
    sorted_idx = np.argsort(minecraft_dict_temp['start_time'])
    if len(sorted_idx) == 3:
        minecraft_dict_temp['state'][2] = 'hands_on_training'
        minecraft_dict_temp['state'][1] = 'saturn_a'
        minecraft_dict_temp['state'][0] = 'saturn_b'
    else:
        #sometimes due to time constraints the participants play only 1 misson
        minecraft_dict_temp['state'][2] = 'hands_on_training'
        minecraft_dict_temp['state'][1] = 'saturn_a'

    for s_idx in sorted_idx:
        minecraft_dict[idx_minecraft_dict] = {'state':minecraft_dict_temp['state'][s_idx], 'participant': None, 
                                'start_time': minecraft_dict_temp['start_time'][s_idx], 
                                'end_time':minecraft_dict_temp['end_time'][s_idx]}
        
        idx_minecraft_dict += 1
    return minecraft_dict
