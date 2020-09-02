import json
import os
from tqdm import tqdm
from enum import Enum
from hackathon.evidence_extraction import save_individual_evidence_set, load_evidence_set

MISSION_TIME_IN_SECONDS = 600
LT_EVIDENCE_FILENAME = "lights"
RM_EVIDENCE_FILENAME = "rooms"
TG_EVIDENCE_FILENAME = "triaging_green"
TY_EVIDENCE_FILENAME = "triaging_yellow"

def read_map(filepath):
    map = {}

    with open(filepath, "r") as input_file:
        json_obj = json.load(input_file)
        for location in json_obj["locations"]:
            if location["type"].startswith("room") or\
               location["type"].startswith("bathroom"):
                map[location["id"]] = 1
            else:
                map[location["id"]] = 0

    return map

def extract_evidence(input_folder, output_folder, map):
    complete_lt_evidence_set = []
    complete_rm_evidence_set = []
    complete_tg_evidence_set = []
    complete_ty_evidence_set = []

    for message_file in tqdm(os.listdir(input_folder), desc="Extracting evidence"):
        if os.path.isdir(os.path.join(input_folder, message_file)) or\
                message_file.startswith("."):
            continue
        with open(os.path.join(input_folder, message_file), "r") as input_file:
            lt_evidence_set = []
            rm_evidence_set = []
            tg_evidence_set = []
            ty_evidence_set = []
            mission_started = False
            previous_time_step = -1
            time_step = 0
            triaging = False
            triaging_green = False
            area_id = "Unknown"

            for data_point in input_file:
                if data_point.strip():
                    message = json.loads(data_point)

                    if message["header"]["message_type"] == "event":
                        if message["msg"]["sub_type"] == "Event:Triage":
                            if mission_started:
                                if message["data"]["triage_state"] == "IN_PROGRESS":
                                    triaging = True
                                    if message["data"]["color"] == "Yellow":
                                        triaging_green = False
                                    else:
                                        triaging_green = True
                                else:
                                    triaging = False
                        elif message["msg"]["sub_type"] == "Event:location":
                            if mission_started:
                                if "locations" in message["data"].keys():
                                    area_id = message["data"]["locations"][0]["id"]
                        elif message["msg"]["sub_type"] == "Event:MissionState":
                            mission_started = True
                    elif message["header"]["message_type"] == "observation":
                        if message["msg"]["sub_type"] == "state":
                            if mission_started:
                                timer = message["data"]["mission_timer"].split(':')
                                minutes = int(timer[0].strip())
                                seconds = int(timer[1].strip())
                                time_step = 600 - (minutes*60 + seconds)
                                if time_step < 0:
                                    time_step = 0

                    if time_step >= 600:
                        break

                    if previous_time_step != time_step:
                        lt_evidence_set.append(1) # lights is always on
                        tg_evidence_set.append(1 if triaging and triaging_green else 0)
                        ty_evidence_set.append(1 if triaging and not triaging_green else 0)
                        if area_id in map.keys():
                            rm_evidence_set.append(map[area_id])
                        else:
                            rm_evidence_set.append(0)

                        previous_time_step = time_step

        complete_lt_evidence_set.append(lt_evidence_set)
        complete_rm_evidence_set.append(rm_evidence_set)
        complete_tg_evidence_set.append(tg_evidence_set)
        complete_ty_evidence_set.append(ty_evidence_set)

    save_individual_evidence_set(
        output_folder, LT_EVIDENCE_FILENAME, complete_lt_evidence_set
    )
    save_individual_evidence_set(
        output_folder, RM_EVIDENCE_FILENAME, complete_rm_evidence_set
    )
    save_individual_evidence_set(
        output_folder, TG_EVIDENCE_FILENAME, complete_tg_evidence_set
    )
    save_individual_evidence_set(
        output_folder, TY_EVIDENCE_FILENAME, complete_ty_evidence_set
    )

if __name__ == "__main__":
    map = read_map('../data/maps/new_format/Falcon.json')
    extract_evidence('../data/experiments/asist/raw/falcon/new_format',
                     '../data/evidence/asist/falcon/new_format',
                     map)
    evidence_set = load_evidence_set('../data/evidence/asist/falcon/new_format', True)
    print('Stop')