import json
import os
from tqdm import tqdm
import datetime

WORLD_TICK_PER_SECONDS = 20
MISSION_TIME_IN_SECONDS = 600
OBSERVATIONS_FILENAME = 'observations.txt'
LEVER_FILENAME = 'lever_event.txt'
TRIAGE_FILENAME = 'triage_event.txt'
ROOM_FILENAME = 'room_event.txt'

def split_data_files_into_folders(input_folder, output_folder, show_progress=True):
    """
    This function reads the data file and divides its content in separate files: observations, triage events,
    room events and lever events in a folder with the same name as the original data file.
    Since the data can be unordered, the contents are reordered by timestamp before being saved.
    """
    for experiment_data_file in tqdm(os.listdir(input_folder), desc='Splitting files into folders',
                                     disable=not show_progress):
        if os.path.isdir(os.path.join(input_folder, experiment_data_file)):
            continue

        # The name of the destiny folder is the same as the name of the file
        experiment_folder = os.path.join(output_folder, experiment_data_file.replace('.json', ''))
        if not os.path.isdir(experiment_folder):
            os.mkdir(experiment_folder)

        observations = []
        lever_events = []
        room_events = []
        triage_events = []
        with open(os.path.join(input_folder, experiment_data_file), 'r') as input_file:
            for data_point in input_file:
                if data_point.strip():
                    message = json.loads(data_point)
                    message['header']['timestamp'] = format_timestamp_string(message['header']['timestamp'])

                    if message['header']['message_type'] == 'event':
                        if message['msg']['sub_type'] == 'Event:Triage':
                            triage_events.append(message)
                        elif message['msg']['sub_type'] == 'Event:location':
                            room_events.append(message)
                        elif message['msg']['sub_type'] == 'Event:Lever':
                            lever_events.append(message)
                    elif message['header']['message_type'] == 'observation':
                        if message['msg']['sub_type'] == 'state':
                            observations.append(message)

        observations = sort_by_timestamp(observations)
        lever_events = sort_by_timestamp(lever_events)
        room_events = sort_by_timestamp(room_events)
        triage_events = sort_by_timestamp(triage_events)

        write_dataset_to_file(observations, os.path.join(experiment_folder, OBSERVATIONS_FILENAME))
        write_dataset_to_file(lever_events, os.path.join(experiment_folder, LEVER_FILENAME))
        write_dataset_to_file(room_events, os.path.join(experiment_folder, ROOM_FILENAME))
        write_dataset_to_file(triage_events, os.path.join(experiment_folder, TRIAGE_FILENAME))

def format_timestamp_string(timestamp):
    """
    This function converts all sorts of timestamp formats found in the game to a unified format
    """
    try:
        formatted_timestamp = datetime.datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%S:%fZ')
    except ValueError:
        try:
            formatted_timestamp = datetime.datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%S.%fZ')
        except ValueError:
            formatted_timestamp = datetime.datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%SZ')

    return datetime.datetime.strftime(formatted_timestamp, '%Y-%m-%dT%H:%M:%S.%fZ')

def sort_by_timestamp(dataset):
    """
    This function sorts the contents of a dataset by the timestamps in their header
    """
    return sorted(dataset, key=lambda x: datetime.datetime.strptime(x['header']['timestamp'], '%Y-%m-%dT%H:%M:%S.%fZ'),
                  reverse=False)

def write_dataset_to_file(dataset, destiny_filepath):
    """
    This function writes a list of json objects data points to a file
    """
    with open(destiny_filepath, 'w') as destiny_file:
        for data_point in dataset:
            destiny_file.write(json.dumps(data_point) + '\n')


if __name__ == '__main__':
    split_data_files_into_folders('../data/experiments/asist/raw/singleplayer',
                                  '../data/experiments/asist/formatted/singleplayer')
    split_data_files_into_folders('../data/experiments/asist/raw/sparky', '../data/experiments/asist/formatted/sparky')
    split_data_files_into_folders('../data/experiments/asist/raw/falcon', '../data/experiments/asist/formatted/falcon')
