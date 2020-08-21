import json
import os
from tqdm import tqdm
import datetime
from hackathon.utils import to_datetime, read_data_from_file
from enum import Enum

WORLD_TICK_PER_SECONDS = 20
MISSION_TIME_IN_SECONDS = 600
OBSERVATIONS_FILENAME = "observations.txt"
LEVER_FILENAME = "lever_event.txt"
TRIAGE_FILENAME = "triage_event.txt"
ROOM_FILENAME = "room_event.txt"
METADATA_FILENAME = "metadata.txt"


class MissionMap(Enum):
    SINGLEPLAYER = 0
    SPARKY = 1
    FALCON = 2
    SHARED = 3


def split_data_files_into_folders(
        input_folder, output_folder, show_progress=True
):
    """
    This function reads the data file and divides its content in separate files: observations, triage events,
    room events and lever events in a folder with the same name as the original data file.
    Since the data can be unordered, the contents are reordered by timestamp before being saved.
    """
    for experiment_data_file in tqdm(
            os.listdir(input_folder),
            desc="Splitting files into folders",
            disable=not show_progress,
    ):
        if os.path.isdir(
                os.path.join(input_folder, experiment_data_file)
        ) or experiment_data_file.startswith("."):
            continue

        # The name of the destiny folder is the same as the name of the file
        experiment_folder = os.path.join(
            output_folder, experiment_data_file.replace(".json", "")
        )
        if not os.path.isdir(experiment_folder):
            os.mkdir(experiment_folder)

        observations = []
        lever_events = []
        room_events = []
        triage_events = []
        mission_start_time = None
        metadata = []
        with open(
                os.path.join(input_folder, experiment_data_file), "r"
        ) as input_file:
            for data_point in input_file:
                if data_point.strip():
                    message = json.loads(data_point)
                    message["header"]["timestamp"] = format_timestamp_string(
                        message["header"]["timestamp"]
                    )

                    if message["header"]["message_type"] == "event":
                        if message["msg"]["sub_type"] == "Event:Triage":
                            triage_events.append(message)
                        elif message["msg"]["sub_type"] == "Event:location":
                            room_events.append(message)
                        elif message["msg"]["sub_type"] == "Event:Lever":
                            lever_events.append(message)
                        elif (
                                message["msg"]["sub_type"] == "Event:MissionState"
                        ):
                            if (
                                    message["data"]["mission_state"].upper()
                                    == "START"
                            ):
                                mission_start_time = to_datetime(
                                    message["header"]["timestamp"]
                                )
                                metadata.append(
                                    {
                                        "start_time": message["header"][
                                            "timestamp"
                                        ]
                                    }
                                )
                    elif message["header"]["message_type"] == "observation":
                        if message["msg"]["sub_type"] == "state":
                            observations.append(message)

        observations = sort_by_timestamp(observations)
        lever_events = sort_by_timestamp(lever_events)
        room_events = sort_by_timestamp(room_events)
        triage_events = sort_by_timestamp(triage_events)

        observations = remove_data_prior_to(mission_start_time, observations)
        lever_events = remove_data_prior_to(mission_start_time, lever_events)
        room_events = remove_data_prior_to(mission_start_time, room_events)
        triage_events = remove_data_prior_to(mission_start_time, triage_events)

        write_dataset_to_file(
            observations,
            os.path.join(experiment_folder, OBSERVATIONS_FILENAME),
        )
        write_dataset_to_file(
            lever_events, os.path.join(experiment_folder, LEVER_FILENAME)
        )
        write_dataset_to_file(
            room_events, os.path.join(experiment_folder, ROOM_FILENAME)
        )
        write_dataset_to_file(
            triage_events, os.path.join(experiment_folder, TRIAGE_FILENAME)
        )
        write_dataset_to_file(
            metadata, os.path.join(experiment_folder, METADATA_FILENAME)
        )


def format_timestamp_string(timestamp):
    """
    This function converts all sorts of timestamp formats found in the game to a unified format
    """
    try:
        formatted_timestamp = datetime.datetime.strptime(
            timestamp, "%Y-%m-%dT%H:%M:%S:%fZ"
        )
    except ValueError:
        try:
            formatted_timestamp = datetime.datetime.strptime(
                timestamp, "%Y-%m-%dT%H:%M:%S.%fZ"
            )
        except ValueError:
            formatted_timestamp = datetime.datetime.strptime(
                timestamp, "%Y-%m-%dT%H:%M:%SZ"
            )

    return datetime.datetime.strftime(
        formatted_timestamp, "%Y-%m-%dT%H:%M:%S.%fZ"
    )


def sort_by_timestamp(dataset):
    """
    This function sorts the contents of a dataset by the timestamps in their header
    """
    return sorted(
        dataset,
        key=lambda x: datetime.datetime.strptime(
            x["header"]["timestamp"], "%Y-%m-%dT%H:%M:%S.%fZ"
        ),
        reverse=False,
    )


def remove_data_prior_to(timestamp, data):
    valid_data = []
    for data_point in data:
        if to_datetime(data_point["header"]["timestamp"]) >= timestamp:
            valid_data.append(data_point)

    return valid_data


def write_dataset_to_file(dataset, destiny_filepath):
    """
    This function writes a list of json objects data points to a file
    """
    with open(destiny_filepath, "w") as destiny_file:
        for data_point in dataset:
            destiny_file.write(json.dumps(data_point) + "\n")


def extract_room_events_from_observations(
        formatted_experiment_data_folder, mission_map_id, show_progress=True
):
    for experiment_folder in tqdm(
            os.listdir(formatted_experiment_data_folder),
            desc="Extracting room entrance events from observations",
            disable=not show_progress,
    ):
        if experiment_folder.startswith("."):
            continue

        observations = read_data_from_file(
            os.path.join(
                formatted_experiment_data_folder,
                experiment_folder,
                OBSERVATIONS_FILENAME,
            )
        )
        previous_area_id = get_initial_area(mission_map_id)
        areas = get_list_of_areas(mission_map_id)

        room_events = []
        for observation in observations:
            new_area_id = get_area_id_by_players_position(
                areas, observation["data"]["x"], observation["data"]["z"]
            )
            if new_area_id != previous_area_id and new_area_id != None:
                # This is a simplified json content with only the fields relevant for the evaluation of the models
                area_event_message = {}
                area_event_message["host"] = observation["host"]
                area_event_message[
                    "topic"
                ] = "observations/events/player/location"
                area_event_message["header"] = {
                    "version": 0.5,
                    "message_type": "event",
                    "timestamp": observation["header"]["timestamp"],
                }
                area_event_message["data"] = {
                    "playername": observation["data"]["name"],
                    "entered_area_id": new_area_id,
                    "exited_area_id": previous_area_id,
                }

                room_events.append(area_event_message)
                previous_area_id = new_area_id

        write_dataset_to_file(
            room_events,
            os.path.join(
                formatted_experiment_data_folder,
                experiment_folder,
                ROOM_FILENAME,
            ),
        )


def get_initial_area(mission_map_id):
    """
    This function retrieves the id of the initial area the player is in according to the mission maps
    """
    if mission_map_id == MissionMap.SINGLEPLAYER:
        return "as"
    elif mission_map_id == MissionMap.SPARKY:
        return "sr"
    elif mission_map_id == MissionMap.FALCON:
        return "ew"

    return None


def get_list_of_areas(mission_map_id):
    """
    This function retrieves the list of areas from a mission maps file config
    """
    if mission_map_id == MissionMap.SINGLEPLAYER:
        map_filepath = "../data/maps/singleplayer.json"
    elif mission_map_id == MissionMap.SPARKY:
        map_filepath = "../data/maps/sparky.json"
    else:
        map_filepath = "../data/maps/falcon.json"

    with open(map_filepath, "r") as input_map_file:
        map = json.load(input_map_file)

    return map["areas"]


def get_area_id_by_players_position(areas, player_x, player_y):
    for area in areas:
        if (
                area["x1"] <= player_x <= area["x2"]
                and area["y1"] <= player_y <= area["y2"]
        ):
            return area["id"]

    return None


if __name__ == "__main__":
    split_data_files_into_folders('../data/experiments/asist/raw/singleplayer',
                                  '../data/experiments/asist/formatted/singleplayer')
    split_data_files_into_folders(
        '../data/experiments/asist/raw/sparky',
        '../data/experiments/asist/formatted/sparky')

    split_data_files_into_folders('../data/experiments/asist/raw/falcon', '../data/experiments/asist/formatted/falcon')

    # The Falcon map has inconsistencies in the room event data so, while this is not fixed, these
    # events will be extracted from the observations of the player's positions in the following method
    extract_room_events_from_observations('../data/experiments/asist/formatted/falcon', MissionMap.FALCON)
