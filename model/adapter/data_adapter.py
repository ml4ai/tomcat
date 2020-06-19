import json
import os
from shutil import move
import pandas as pd
import datetime
from tqdm import tqdm
import shlex

"""
This script transforms the data collected in our internal experiments to the testbed format.
It also transforms the testbed formatted data to facilitate the usage of this data in our model.
"""

WORLD_TICK_PER_SECONDS = 20
MISSION_TIME_IN_SECONDS = 900
ROOMS = ['aw', 'as', 'achl', 'ach', 'alha', 'arha', 'alhb', 'arhb', 'awb', 'amb', 'ae1', 'ae2', 'ar201', 'ar203',
         'ar205', 'ar207', 'ajc', 'ar208', 'ar210', 'ar209', 'ar211', 'ar213', 'ar215', 'ar216', 'ar218', 'ar220']
ROOM_LOCATIONS = {'aw': {'x1': -2188.7, 'y1': 174.3, 'x2': -2182.3, 'y2': 180.7},
                  'as': {'x1': -2166.7, 'y1': 173.29073, 'x2': -2153.289, 'y2': 181.7},
                  'achl': {'x1': -2151.7, 'y1': 172.3, 'x2': -2140.1404, 'y2': 177.7},
                  'ach': {'x1': -2139.571, 'y1': 174.3, 'x2': -2108.3, 'y2': 177.7},
                  'alha': {'x1': -2139.7, 'y1': 156.96628, 'x2': -2136.3, 'y2': 173.68864},
                  'arha': {'x1': -2139.7, 'y1': 178.15, 'x2': -2136.3, 'y2': 194.94315},
                  'alhb': {'x1': -2139.7, 'y1': 153.3, 'x2': -2108.3, 'y2': 156.7},
                  'arhb': {'x1': -2139.7, 'y1': 195.3, 'x2': -2108.3, 'y2': 198.7},
                  'awb': {'x1': -2149.7, 'y1': 159.3, 'x2': -2146.3, 'y2': 170.7},
                  'amb': {'x1': -2144.7, 'y1': 159.3, 'x2': -2141.3, 'y2': 170.7},
                  'ae1': {'x1': -2145.7, 'y1': 179.2756, 'x2': -2144.3, 'y2': 182.7},
                  'ae2': {'x1': -2148.7, 'y1': 179.27821, 'x2': -2147.3, 'y2': 182.7},
                  'ar201': {'x1': -2134.7, 'y1': 158.3, 'x2': -2129.3, 'y2': 164.7},
                  'ar203': {'x1': -2127.7, 'y1': 158.3, 'x2': -2123.3, 'y2': 164.7},
                  'ar205': {'x1': -2121.7, 'y1': 158.35489, 'x2': -2117.3, 'y2': 164.7},
                  'ar207': {'x1': -2115.7, 'y1': 158.3, 'x2': -2108.2217, 'y2': 164.7},
                  'ajc': {'x1': -2134.7, 'y1': 166.3, 'x2': -2133.3, 'y2': 171.7},
                  'ar208': {'x1': -2131.7, 'y1': 166.3, 'x2': -2116.3, 'y2': 172.7},
                  'ar210': {'x1': -2114.7, 'y1': 166.3, 'x2': -2107.8625, 'y2': 172.7},
                  'ar209': {'x1': -2134.7, 'y1': 179.3, 'x2': -2130.3, 'y2': 185.7},
                  'ar211': {'x1': -2128.7, 'y1': 179.3, 'x2': -2124.3, 'y2': 185.7},
                  'ar213': {'x1': -2122.7, 'y1': 180.3, 'x2': -2116.3, 'y2': 185.7},
                  'ar215': {'x1': -2114.7, 'y1': 179.3, 'x2': -2108.2644, 'y2': 185.7},
                  'ar216': {'x1': -2134.7, 'y1': 187.3, 'x2': -2123.3, 'y2': 193.7},
                  'ar218': {'x1': -2121.7, 'y1': 187.3, 'x2': -2116.3, 'y2': 193.7},
                  'ar220': {'x1': -2114.7, 'y1': 187.3, 'x2': -2108.3, 'y2': 193.7}}

# For the victims, the key is their coordinates and the value is their indices
GREEN_VICTIMS = {(-2145, 182): 0,  # ae1
                 (-2148, 160): 1,  # awb
                 (-2142, 165): 2,  # amb
                 (-2142, 161): 3,  # amb
                 (-2133, 164): 4,  # ar201
                 (-2130, 162): 5,  # ar201
                 (-2124, 164): 6,  # ar203
                 (-2116, 160): 7,  # ar207
                 (-2109, 158): 8,  # ar207
                 (-2115, 166): 9,  # ar210
                 (-2110, 171): 10,  # ar210
                 (-2125, 184): 11,  # ar211
                 (-2118, 180): 12,  # ar213
                 (-2120, 185): 13,  # ar213
                 (-2115, 181): 14,  # ar215
                 (-2124, 188): 15,  # ar216
                 (-2119, 187): 16,  # ar218
                 (-2113, 190): 17,  # ar220
                 (-2110, 188): 18,  # ar220
                 }

YELLOW_VICTIMS = {(-2148, 180): 0,  # ae2
                  (-2135, 166): 1,  # ajc
                  (-2110, 164): 2,  # ar207
                  (-2125, 171): 3,  # ar208
                  (-2118, 168): 4,  # ar208
                  (-2109, 185): 5,  # ar215
                  (-2134, 192): 6,  # ar216
                  }

def get_room_index_by_player_position(x, y):
    room_id = get_room_by_player_position(x, y)
    return ROOMS.index(room_id)

def get_room_by_player_position(x, y):
    epsilon = 1  # Variance of 1 block
    for room, location in ROOM_LOCATIONS.items():
        if location['x1'] - epsilon <= x <= location['x2'] + epsilon and location['y1'] - epsilon <= y <= location[
            'y2'] + epsilon:
            return room

def read_evidence_data_from_file(filepath):
    data = pd.read_csv(filepath, index_col=0)
    data.columns = [eval(column) for column in data.columns]
    return data

def read_parameters_from_file(filepath):
    parameter_values = pd.read_csv(filepath, index_col=0).transpose().iloc[0]
    for node_id in parameter_values.index:
        value = [float(v) for v in shlex.split(parameter_values[node_id].strip('[]'))]
        if len(value) == 1:
            value = value[0]
        parameter_values[node_id] = value

    return parameter_values

class InternalDataAdapter:

    def __init__(self, internal_data_folder):
        self.internal_data_folder = internal_data_folder

    def convert_to_testbed_format(self, testbed_folder):
        for experiment_folder in tqdm(os.listdir(self.internal_data_folder), desc='Converting to Testbed Format'):
            if experiment_folder.startswith('.'):
                continue

            testbed_experiment_folder = '{}/{}'.format(testbed_folder, experiment_folder)

            if not os.path.isdir(testbed_experiment_folder):
                os.mkdir(testbed_experiment_folder)

            events_file = '{}/{}/mission_2/events.txt'.format(self.internal_data_folder, experiment_folder)
            self.__convert_lever_and_triage_events(testbed_experiment_folder, events_file)

            observations_file = '{}/{}/mission_2/malmo_data.txt'.format(self.internal_data_folder, experiment_folder)
            self.__convert_observations(testbed_experiment_folder, observations_file)

    def __convert_lever_and_triage_events(self, output_folder, events_file):
        with open(events_file, 'r') as input_file:
            with open('{}/{}'.format(output_folder, TestbedDataAdapter.LEVER_EVENT_FILENAME), 'w') as lever_file:
                with open('{}/{}'.format(output_folder, TestbedDataAdapter.TRIAGE_EVENT_FILENAME), 'w') as triage_file:
                    for json_obj in input_file:
                        if json_obj.strip():
                            event = json.loads(json_obj)
                            testbed_message = {}
                            testbed_message['header'] = {'timestamp': self.__format_timestamp(event['timestamp']),
                                                         'message_type': 'event',
                                                         'version': 'na'}

                            if event['event_type'] == 'edu.arizona.tomcat.Events.LeverFlip':
                                testbed_message['msg'] = {'experiment_id': 'na', 'trial_id': 'na',
                                                          'timestamp': self.__format_timestamp(event['timestamp']),
                                                          'source': 'simulator', 'sub_type': 'Event:Lever',
                                                          'version': 'na'}
                                testbed_message['data'] = {'playername': event['player_name'],
                                                           'powered': event['was_powered'],
                                                           'lever_x': event['block_position']['x'],
                                                           'lever_y': event['block_position']['y'],
                                                           'lever_z': event['block_position']['z']}

                                lever_file.write(json.dumps(testbed_message) + '\n')

                            elif event['event_type'] == 'edu.arizona.tomcat.Events.BlockInteraction' and (event[
                                                                                                              'block_material'] == 'minecraft:prismarine' or
                                                                                                          event[
                                                                                                              'block_material'] == 'minecraft:gold_block'):

                                testbed_message['msg'] = {'experiment_id': 'na', 'trial_id': 'na',
                                                          'timestamp': self.__format_timestamp(event['timestamp']),
                                                          'source': 'simulator', 'sub_type': 'Event:Triage',
                                                          'version': 'na'}
                                testbed_message['data'] = {'playername': event['player_name'],
                                                           'triage_state': 'IN_PROGRESS',
                                                           'victim_x': event['block_position']['x'],
                                                           'victim_y': event['block_position']['y'],
                                                           'victim_z': event['block_position']['z']}

                                triage_file.write(json.dumps(testbed_message) + '\n')

    def __format_timestamp(self, timestamp):
        try:
            formatted_timestamp = datetime.datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%S:%fZ')
        except ValueError:
            try:
                formatted_timestamp = datetime.datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%S.%fZ')
            except ValueError:
                formatted_timestamp = datetime.datetime.strptime(timestamp, '%Y-%m-%dT%H:%M:%SZ')

        return datetime.datetime.strftime(formatted_timestamp, '%Y-%m-%dT%H:%M:%S.%fZ')

    def __convert_observations(self, output_folder, observations_file):
        """
        Adjusts timestamps in the header and move the player's name to the object data
        to comply with the format provided by the testbed
        """
        with open(observations_file, 'r') as input_file:
            with open('{}/{}'.format(output_folder, TestbedDataAdapter.OBSERVATIONS_FILENAME), 'w') as out_obs_file:
                for json_obj in input_file:
                    if json_obj.strip():
                        observation = json.loads(json_obj)

                        testbed_message = {}
                        timestamp = self.__format_timestamp(observation['header']['timestamp'])
                        testbed_message['header'] = observation['header']
                        testbed_message['header']['timestamp'] = timestamp
                        testbed_message['msg'] = observation['msg']
                        testbed_message['msg']['timestamp'] = timestamp
                        testbed_message['data'] = observation['data']
                        testbed_message['data']['timestamp'] = timestamp

                        out_obs_file.write(json.dumps(testbed_message) + '\n')

class TestbedDataAdapter:
    OBSERVATIONS_FILENAME = 'observations.txt'
    LEVER_EVENT_FILENAME = 'lever_event.txt'
    TRIAGE_EVENT_FILENAME = 'triage_event.txt'
    ROOM_EVENT_FILENAME = 'room_event.txt'

    def __init__(self, testbed_data_folder):
        self.testbed_data_folder = testbed_data_folder

    def adapt(self):
        self.constrain_observations_to_mission_time()
        self.extract_room_entrance_events_from_observations()
        # self.extract_victims_distance_from_observations()
        self.add_world_tick_to_events()

    def constrain_observations_to_mission_time(self):
        """
        Only keep observations from the moment the player enters the Staging Area until 10 minutes
        has passed in world ticks
        """
        for experiment_folder in tqdm(os.listdir(self.testbed_data_folder),
                                      desc='Constraining observations to mission time'):
            if experiment_folder.startswith('.'):
                continue

            testbed_experiment_folder = '{}/{}'.format(self.testbed_data_folder, experiment_folder)
            observations_file = '{}/observations.txt'.format(testbed_experiment_folder, experiment_folder)
            observations_temp_file = '{}/temp.txt'.format(testbed_experiment_folder, experiment_folder)
            current_room = 'aw'
            initial_time = None
            with open(observations_file, 'r') as input_file:
                with open(observations_temp_file, 'w') as temp_out_file:
                    for json_obj in input_file:
                        observation = json.loads(json_obj)
                        room = get_room_by_player_position(observation['data']['x'], observation['data']['z'])
                        if room == 'aw' and current_room != 'aw':
                            break
                        elif initial_time != None and observation['data'][
                            'total_time'] - initial_time > WORLD_TICK_PER_SECONDS * MISSION_TIME_IN_SECONDS:
                            break
                        elif room != 'aw':
                            if initial_time == None:
                                initial_time = observation['data']['total_time']
                            temp_out_file.write(json.dumps(observation) + '\n')

                        current_room = room

            # Override previous observations file with the new one
            move(observations_temp_file, observations_file)

    def extract_room_entrance_events_from_observations(self):
        for experiment_folder in tqdm(os.listdir(self.testbed_data_folder), desc='Collecting room entrance events'):
            if experiment_folder.startswith('.'):
                continue

            testbed_experiment_folder = '{}/{}'.format(self.testbed_data_folder, experiment_folder)
            observations_file = '{}/observations.txt'.format(testbed_experiment_folder)
            room_event_file = '{}/{}'.format(testbed_experiment_folder, TestbedDataAdapter.ROOM_EVENT_FILENAME)
            current_room = 'aw'
            with open(observations_file, 'r') as input_file:
                with open(room_event_file, 'w') as room_file:
                    for json_obj in input_file:
                        observation = json.loads(json_obj)
                        room = get_room_by_player_position(observation['data']['x'], observation['data']['z'])
                        if room == 'aw' and current_room != 'aw':
                            break  # end of the game
                        else:
                            if room != current_room:
                                testbed_message = {}
                                testbed_message['header'] = {'timestamp': observation['header']['timestamp'],
                                                             'message_type': 'event', 'version': 'na'}
                                testbed_message['msg'] = {'experiment_id': 'na', 'trial_id': 'na',
                                                          'timestamp': observation['msg']['timestamp'],
                                                          'source': 'IHMCLocationMonitorAgent',
                                                          'sub_type': 'Event:Location',
                                                          'version': 'na'}
                                testbed_message['data'] = {'playername': observation['data']['name'],
                                                           'entered_area_id': room,
                                                           'entered_area_name': 'na', 'exited_area_id': current_room,
                                                           'exited_area_name': 'na'}

                                room_file.write(json.dumps(testbed_message) + '\n')
                                current_room = room

    def add_world_tick_to_events(self):
        for experiment_folder in tqdm(os.listdir(self.testbed_data_folder), desc='Adding world ticks to events'):
            if experiment_folder.startswith('.'):
                continue

            times_df = self.__get_observations_time_as_dataframe(experiment_folder)
            self.__add_world_tick_to_event(experiment_folder, TestbedDataAdapter.ROOM_EVENT_FILENAME, times_df)
            self.__add_world_tick_to_event(experiment_folder, TestbedDataAdapter.LEVER_EVENT_FILENAME, times_df)
            self.__add_world_tick_to_event(experiment_folder, TestbedDataAdapter.TRIAGE_EVENT_FILENAME, times_df)

    def __get_observations_time_as_dataframe(self, experiment_folder):
        testbed_experiment_folder = '{}/{}'.format(self.testbed_data_folder, experiment_folder)
        observations_file = '{}/observations.txt'.format(testbed_experiment_folder, experiment_folder)
        observation_times = []
        with open(observations_file, 'r') as input_file:
            for json_obj in input_file:
                observation = json.loads(json_obj)
                observation_time = {}
                timestamp = datetime.datetime.strptime(observation['header']['timestamp'], '%Y-%m-%dT%H:%M:%S.%fZ')
                observation_time['timestamp'] = observation['header']['timestamp']
                observation_time['total_time'] = observation['data']['total_time']
                observation_times.append(observation_time)

        dataframe = pd.DataFrame(observation_times)
        dataframe['timestamp'] = pd.to_datetime(dataframe['timestamp'], format='%Y-%m-%dT%H:%M:%S.%fZ')
        dataframe.set_index('timestamp', inplace=True)
        # Remove duplicate times
        dataframe = dataframe.loc[~dataframe.index.duplicated(keep='first')]

        return dataframe

    def __add_world_tick_to_event(self, experiment_folder, event_filename, times_df):
        testbed_experiment_folder = '{}/{}'.format(self.testbed_data_folder, experiment_folder)
        event_file = '{}/{}'.format(testbed_experiment_folder, event_filename)
        event_temp_file = '{}/temp.txt'.format(testbed_experiment_folder, 'lever')

        with open(event_file, 'r') as input_file:
            with open(event_temp_file, 'w') as temp_out_file:
                for json_obj in input_file:
                    event = json.loads(json_obj)
                    event_timestamp = datetime.datetime.strptime(event['header']['timestamp'], '%Y-%m-%dT%H:%M:%S.%fZ')
                    event['header']['total_time'] = int(
                        times_df.iloc[times_df.index.get_loc(event_timestamp, method='nearest')]['total_time'])

                    temp_out_file.write(json.dumps(event) + '\n')

        # Override previous event file with the new one
        move(event_temp_file, event_file)

if __name__ == '__main__':
    internal_data_folder = '../data/input/cleaned_data_deidentified'
    testbed_folder = '../data/input/testbed'

    internal_data_adapter = InternalDataAdapter(internal_data_folder)
    internal_data_adapter.convert_to_testbed_format(testbed_folder)

    testbed_data_adapter = TestbedDataAdapter(testbed_folder)
    testbed_data_adapter.adapt()
