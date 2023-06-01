import json

def read_minecraft_timestamps(block):
    for i in range(0,len(block)):
        if block[i]['info']['name'][0] == 'Minecraft':
            condition = []
            TrialMessages = []
            for i, line in enumerate(block[i]['time_series']):
                try:
                    json_message = json.loads(line[0])
                    TrialMessages.append(json_message)
                    if (
                        json_message["header"]["message_type"] == "event"
                        and json_message["msg"]["sub_type"] == "Event:MissionState"
                    ):
                        if json_message["data"]["mission_state"] == "Start":
                            trial_start = json_message["msg"]["timestamp"]
                            condition.append('Start')
                            print('Mission start:', trial_start, i)
                        if json_message["data"]["mission_state"] == "Stop":
                            trial_end = json_message["msg"]["timestamp"]
                            condition.append('Stop')
                            print('Mission end:', trial_end, i)
                            
                    if len(condition) == 2:
                        continue
                    else: 
                        #if mission end doesn't exist, then then look for trial end.
                        if (
                            json_message["header"]["message_type"] == "trial"
                            ):
                            print(json_message["msg"]["sub_type"])
                            if json_message["msg"]["sub_type"] == "stop":
                                trial_end = json_message["msg"]["timestamp"]
                                print('Trial end:', trial_end, i)
                            
                except:
                    continue