import json

def read_minecraft_timestamps(block, PingPong_markers):
    idx = len(PingPong_markers)
    markers = PingPong_markers

    trial_data = {}
    mission_start = None
    TrialMessages = []
    condition = []
    for i in range(len(block)):
        if block[i]['info']['name'][0] == 'Minecraft':
            for ind, line in enumerate(block[i]['time_series']):
                try:
                    json_message = json.loads(line[0])
                    TrialMessages.append(json_message)
                    
                    if (
                        json_message["header"]["message_type"] == "event"
                        and json_message["msg"]["sub_type"] == "Event:MissionState"
                    ):
                        if json_message["data"]["mission_state"] == "Start":
                            mission_start = json_message["msg"]["timestamp"]
                            condition.append('Start')
                            mission_start_inx = block[i]['time_stamps'][ind]
                            print('Mission start:', mission_start, ind)
                            
                        if json_message["data"]["mission_state"] == "Stop":
                            mission_end = json_message["msg"]["timestamp"]
                            condition.append('Stop')
                            print('Mission end:', mission_end, ind)
                            mission_end_inx = block[i]['time_stamps'][ind]
                            if mission_start not in trial_data:
                                trial_data[mission_start_inx] = mission_end_inx
                    
                    if len(condition) == 2:
                        continue
                    else:
                        if (
                            json_message["header"]["message_type"] == "trial"
                        ):
                            print(json_message["msg"]["sub_type"])
                            if json_message["msg"]["sub_type"] == "stop":
                                trial_end = json_message["msg"]["timestamp"]
                                print('Trial end:', trial_end, ind)
                                trial_end_inx = block[i]['time_stamps'][ind]
                                if mission_start not in trial_data:
                                    trial_data[mission_start_inx] = trial_end_inx
                                    mission_start = None
                            
                except:
                    continue

    updated_trial_data = []

    for key, value in trial_data.items():
        entry = {
            "start_time": key,
            "end_time": value
        }
        updated_trial_data.append(entry)

    trial_data = updated_trial_data

    sorted_trial_data = sorted(trial_data, key=lambda x: x['start_time'])

    final_data = []

    states = ["hands_on_training", "saturn_a", "saturn_b"]

    for index, data in enumerate(sorted_trial_data):
        markers[idx] = {
            "state": states[index],
            "participant": None,
            "start_time": data["start_time"],
            "end_time": data["end_time"]
        }
        idx += 1

    print(markers)
    return markers