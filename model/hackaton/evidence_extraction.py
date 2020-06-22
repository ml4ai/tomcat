import os
from tqdm import tqdm
import numpy as np
import json
from hackaton import ta3_data_adapter as ta3_data_adapter
import datetime
from enum import Enum

# Observable nodes
LT_EVIDENCE_FILENAME = 'lights'
RM_EVIDENCE_FILENAME = 'rooms'
TG_EVIDENCE_FILENAME = 'triaging_green'
TY_EVIDENCE_FILENAME = 'triaging_yellow'

# Parameter nodes
THETA_S_FILENAME = 'theta_s'
PI_LT_FILENAME = 'pi_lt'
THETA_RM_FILENAME = 'theta_rm'
PI_TG_FILENAME = 'pi_tg'
PI_TY_FILENAME = 'pi_ty'

SINGLEPLAYER_AREA_ORDER = ['as', 'achl', 'ach', 'alha', 'arha', 'alhb', 'arhb', 'awb', 'amb', 'ae1', 'ae2', 'ar201',
                           'ar203', 'ar205', 'ar207', 'ajc', 'ar208', 'ar210', 'ar209', 'ar211', 'ar213', 'ar215',
                           'ar216', 'ar218', 'ar220']
SPARKY_AREA_ORDER = ['sr', 'chl', 'chlb', 'ch', 'lh1', 'rh1', 'lh2', 'rh2', 'wb', 'mb', 'e1', 'e2', 'r201', 'r203',
                     'r205', 'r207', 'jc', 'r208', 'r210', 'r209', 'r211', 'r213', 'r215', 'r216', 'r218', 'r220']
FALCON_AREA_ORDER = []


# todo - define an order for the areas in the Falcon map

class MissionMap(Enum):
    SINGLEPLAYER = 0
    SPARKY = 1
    FALCON = 2


class EvidenceSet():
    """
    This class stores evidence data for the observable nodes used in the model
    """

    def __init__(self, lt_evidence, rm_evidence, tg_evidence, ty_evidence):
        self.lt_evidence = lt_evidence
        self.rm_evidence = rm_evidence
        self.tg_evidence = tg_evidence
        self.ty_evidence = ty_evidence

        if len(self.lt_evidence.shape) == 2:
            self.number_of_data_points, self.time_slices = lt_evidence.shape
        else:
            self.number_of_data_points = 1
            self.time_slices = len(lt_evidence)


def convert_experiments_data_to_evidence_set(experiments_folder, evidence_set_folder, mission_map_id, time_gap,
                                             time_slices, show_progress=True):
    """
    This function converts the raw data from the testbed to a format that can be handled by the ToMCAT model.
    Each row is composed by observations from a complete mission and each column represents the time step when
    that observation took place.
    """
    lt_evidence_set = []
    rm_evidence_set = []
    tg_evidence_set = []
    ty_evidence_set = []

    for experiment_id in tqdm(os.listdir(experiments_folder), desc='Extracting evidence set from data',
                              disable=not show_progress):
        lt, rm, tg, ty = get_evidence_from_experiment(os.path.join(experiments_folder, experiment_id), mission_map_id,
                                                      time_gap, time_slices)
        lt_evidence_set.append(lt)
        rm_evidence_set.append(rm)
        tg_evidence_set.append(tg)
        ty_evidence_set.append(ty)

    save_evidence_set(evidence_set_folder, LT_EVIDENCE_FILENAME, lt_evidence_set)
    save_evidence_set(evidence_set_folder, RM_EVIDENCE_FILENAME, rm_evidence_set)
    save_evidence_set(evidence_set_folder, TG_EVIDENCE_FILENAME, tg_evidence_set)
    save_evidence_set(evidence_set_folder, TY_EVIDENCE_FILENAME, ty_evidence_set)


def get_evidence_from_experiment(experiment_folder, mission_map_id, time_gap, time_slices):
    """
    This function extracts observations along time for a single experiment. The observations are measured from time to
    time, defined by the time_gap variable. Since the timestamps in the data may not match perfectly with the time gap
    defined, the last measurement before a given timestamp is used as observed value for that specific timestamp.
    """
    observations = read_data_from_file(os.path.join(experiment_folder, ta3_data_adapter.OBSERVATIONS_FILENAME))
    lever_events = read_data_from_file(os.path.join(experiment_folder, ta3_data_adapter.LEVER_FILENAME))
    room_events = read_data_from_file(os.path.join(experiment_folder, ta3_data_adapter.ROOM_FILENAME))
    triage_events = read_data_from_file(os.path.join(experiment_folder, ta3_data_adapter.TRIAGE_FILENAME))

    areas = get_list_of_areas(mission_map_id)
    area_lit = get_initial_area_lit_map(mission_map_id)
    current_time_slice = get_mission_start_timestamp(experiment_folder, mission_map_id)

    # most_recent_observation = observations[0]
    area_index = 0
    triaging_green = False
    triaging_yellow = False

    observation_counter = 0
    lever_counter = 0
    room_counter = 0
    triage_counter = 0

    lt_evidence = np.zeros(time_slices, dtype=np.int)
    rm_evidence = np.zeros(time_slices, dtype=np.int)
    tg_evidence = np.zeros(time_slices, dtype=np.int)
    ty_evidence = np.zeros(time_slices, dtype=np.int)

    for t in range(time_slices):
        while to_datetime(observations[observation_counter]['header']['timestamp']) <= current_time_slice:
            # todo - so far we don't neex to extract information from the observations. Later this will be necessary.
            #  However, keep this loop to check whether the mission was executed completely. There will be an error
            #  when there's no observations to read but the number of time slices is still small
            # most_recent_observation = observations[observation_counter]
            observation_counter += 1

        # Get room event in the time slice
        while room_counter < len(room_events) and to_datetime(
                room_events[room_counter]['header']['timestamp']) <= current_time_slice:
            area_id = room_events[room_counter]['data']['entered_area_id']
            if area_id in areas:
                area_index = areas.index(room_events[room_counter]['data']['entered_area_id'])

            room_counter += 1

        # Get light event in the time slice
        while lever_counter < len(lever_events) and to_datetime(lever_events[lever_counter]['header'][
                                                                    'timestamp']) <= current_time_slice:
            area_lit[area_index] = lever_events[lever_counter]['data']['powered']
            lever_counter += 1

        # Get triaging event in the time slice
        while triage_counter < len(triage_events) and to_datetime(triage_events[triage_counter]['header'][
                                                                      'timestamp']) <= current_time_slice:
            if triage_events[triage_counter]['data']['triage_state'] == 'IN_PROGRESS':
                if triage_events[triage_counter]['data']['color'] == 'Green':
                    triaging_green = True
                else:
                    triaging_yellow = True
            else:
                triaging_green = False
                triaging_yellow = False

            triage_counter += 1

        lt_evidence[t] = 1 if area_lit[area_index] else 0
        rm_evidence[t] = area_index
        tg_evidence[t] = 1 if triaging_green else 0
        ty_evidence[t] = 1 if triaging_yellow else 0

        current_time_slice = current_time_slice + datetime.timedelta(0, time_gap)

    return lt_evidence, rm_evidence, tg_evidence, ty_evidence


def read_data_from_file(filepath):
    """
    This function reads data from the testbed saved into a file
    """
    data = []
    with open(filepath, 'r') as file:
        for json_obj in file:
            data.append(json.loads(json_obj))

    return data


def get_initial_area_lit_map(mission_map_id):
    """
    This function returns the initial mapping for the areas that are lit when the mission starts
    """
    area_lit = []

    if mission_map_id == MissionMap.SINGLEPLAYER:
        area_lit = [False] * get_number_of_areas(mission_map_id)
        area_lit[0] = True  # staging room
        area_lit[2:11] = [True] * 9  # These rooms are always lit
    elif mission_map_id == MissionMap.SPARKY:
        area_lit = [False] * get_number_of_areas(mission_map_id)
        area_lit[0] = True  # staging room
        area_lit[3:12] = [True] * 9  # These rooms are always lit
    else:
        pass
        # todo - fill this for the Falcon map later

    return area_lit


def get_number_of_areas(mission_map_id):
    """
    This function returns the number of areas based on the mission map
    """
    if mission_map_id == MissionMap.SINGLEPLAYER:
        return len(SINGLEPLAYER_AREA_ORDER)
    elif mission_map_id == MissionMap.SPARKY:
        return len(SPARKY_AREA_ORDER)
    else:
        return len(FALCON_AREA_ORDER)


def get_mission_start_timestamp(experiment_folder, mission_map_id):
    """
    This function checks the timestamp when the player left the starting area which is when the mission effectively
    starts
    """
    room_event_filename = os.path.join(experiment_folder, ta3_data_adapter.ROOM_FILENAME)
    with open(room_event_filename) as room_event_file:
        for event in room_event_file:
            event_json = json.loads(event)

            if 'entered_area_id' in event_json['data'].keys():
                if event_json['data']['entered_area_id'] == get_initial_area(mission_map_id):
                    return to_datetime(event_json['header']['timestamp'])


def get_initial_area(mission_map_id):
    """
    This function retrieves the id of the initial area the player is in according to the mission maps
    """
    if mission_map_id == MissionMap.SINGLEPLAYER:
        return SINGLEPLAYER_AREA_ORDER[0]
    elif mission_map_id == MissionMap.SPARKY:
        return SPARKY_AREA_ORDER[0]
    else:
        return FALCON_AREA_ORDER[0]


def get_list_of_areas(mission_map_id):
    """
    This function returns the list of ordered areas for a mission map
    """
    if mission_map_id == MissionMap.SINGLEPLAYER:
        return SINGLEPLAYER_AREA_ORDER
    elif mission_map_id == MissionMap.SPARKY:
        return SPARKY_AREA_ORDER
    else:
        return FALCON_AREA_ORDER


# def get_list_of_areas(mission_map_id):
#     """
#     This function retrieves the list of areas from a mission maps file config
#     """
#     areas = {}
#     if mission_map_id == MissionMap.SINGLEPLAYER:
#         map_filepath = '../data/maps/singleplayer.json'
#         areas = {id:{} for id in SINGLEPLAYER_AREA_ORDER}
#     elif mission_map_id == MissionMap.SPARKY:
#         map_filepath = '../data/maps/sparky.json'
#         areas = {id: {} for id in SPARKY_AREA_ORDER}
#     else:
#         map_filepath = '../data/maps/falcon.json'
#         areas = {id: {} for id in FALCON_AREA_ORDER}
#
#     with open(map_filepath, 'r') as input_map_file:
#         map = json.load(input_map_file)
#
#     for area in map['areas']:
#         if area['id'] in areas.keys():
#             areas[area['id']] = {'x1': area['x1'], 'y1': area['y1'], 'x2': area['x2'], 'y2': area['y2']}
#
#     return areas

def to_datetime(timestamp_string):
    return datetime.datetime.strptime(timestamp_string, '%Y-%m-%dT%H:%M:%S.%fZ')


def get_area_by_player_position(areas, x, y):
    """
    This function checks the area the player is in given its position in the maps
    """
    epsilon = 1  # Variance of 1 block
    for i, area in areas:
        if area['x1'] - epsilon <= x <= area['x2'] + epsilon and area['y1'] - epsilon <= y <= area['y2'] + epsilon:
            return i, area['id']


def save_evidence_set(destiny_folder, filename, evidence_set):
    """
    This function saves an evidence set to a file
    """
    np.savetxt(get_evidence_file_absolute_path(destiny_folder, filename), evidence_set, fmt='%i')

def get_evidence_file_absolute_path(destiny_folder, filename):
    """
    Retrieves the absolute path for an evidence file
    """
    return os.path.join(destiny_folder, filename + '.csv')


def load_evidence_set(evidence_folder):
    """
    Loads evidence from files to matrices
    """
    lt_evidence_set = np.loadtxt(get_evidence_file_absolute_path(evidence_folder, LT_EVIDENCE_FILENAME), dtype=np.int)
    rm_evidence_set = np.loadtxt(get_evidence_file_absolute_path(evidence_folder, RM_EVIDENCE_FILENAME), dtype=np.int)
    tg_evidence_set = np.loadtxt(get_evidence_file_absolute_path(evidence_folder, TG_EVIDENCE_FILENAME), dtype=np.int)
    ty_evidence_set = np.loadtxt(get_evidence_file_absolute_path(evidence_folder, TY_EVIDENCE_FILENAME), dtype=np.int)

    return EvidenceSet(lt_evidence_set, rm_evidence_set, tg_evidence_set, ty_evidence_set)

if __name__ == '__main__':
    TIME_GAP = 1
    TIME_SLICES = 600
    convert_experiments_data_to_evidence_set('../data/experiments/asist/formatted/singleplayer',
                                             '../data/evidence/asist/sparky', MissionMap.SINGLEPLAYER, TIME_GAP,
                                             TIME_SLICES)
    convert_experiments_data_to_evidence_set('../data/experiments/asist/formatted/sparky',
                                             '../data/evidence/asist/sparky', MissionMap.SPARKY, TIME_GAP,
                                             TIME_SLICES)
    convert_experiments_data_to_evidence_set('../data/experiments/asist/formatted/falcon',
                                             '../data/evidence/asist/sparky', MissionMap.FALCON, TIME_GAP,
                                             TIME_SLICES)
