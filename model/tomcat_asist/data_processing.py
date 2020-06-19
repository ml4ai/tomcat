import numpy as np
import os
from tqdm import tqdm
import adapter.data_adapter as data_adapter
import json


class EvidenceDataSet():

    def __init__(self, lt_evidence, rm_evidence, tg_evidence, ty_evidence, dg_evidence, dy_evidence):
        self.lt_evidence = lt_evidence
        self.rm_evidence = rm_evidence
        self.tg_evidence = tg_evidence
        self.ty_evidence = ty_evidence
        self.dg_evidence = dg_evidence
        self.dy_evidence = dy_evidence
        self.number_of_data_points, self.time_slices = lt_evidence.shape


class DataProcessing():
    LT_EVIDENCE_FILENAME = 'lights'
    RM_EVIDENCE_FILENAME = 'rooms'
    TG_EVIDENCE_FILENAME = 'triaging_green'
    TY_EVIDENCE_FILENAME = 'triaging_yellow'
    DG_EVIDENCE_FILENAME = 'distance_green'
    DY_EVIDENCE_FILENAME = 'distance_yellow'

    THETA_S_FILENAME = 'theta_s'
    PI_LT_FILENAME = 'pi_lt'
    THETA_RM_FILENAME = 'theta_rm'
    PI_TG_FILENAME = 'pi_tg'
    PI_TY_FILENAME = 'pi_ty'
    SIGMA_DG_FILENAME = 'sigma_dg'
    SIGMA_DY_FILENAME = 'sigma_dy'

    def convert_experiments_data_to_evidence(self, experiment_data_folder, evidence_folder):
        lt_evidence_set = []
        rm_evidence_set = []
        tg_evidence_set = []
        ty_evidence_set = []
        dg_evidence_set = []
        dy_evidence_set = []
        for experiment_id in tqdm(os.listdir(experiment_data_folder), desc='Converting evidence data'):
            lt_evidence, rm_evidence, tg_evidence, ty_evidence, dg_evidence, dy_evidence = self.__collect_evidence_from_experiment_data(
                experiment_data_folder, experiment_id)
            lt_evidence_set.append(lt_evidence)
            rm_evidence_set.append(rm_evidence)
            tg_evidence_set.append(tg_evidence)
            ty_evidence_set.append(ty_evidence)
            dg_evidence_set.append(dg_evidence)
            dy_evidence_set.append(dy_evidence)

        lt_evidence_set = np.stack(lt_evidence_set)
        rm_evidence_set = np.stack(rm_evidence_set)
        tg_evidence_set = np.stack(tg_evidence_set)
        ty_evidence_set = np.stack(ty_evidence_set)
        dg_evidence_set = np.stack(dg_evidence_set, axis=1)
        dy_evidence_set = np.stack(dy_evidence_set, axis=1)

        evidence_set = EvidenceDataSet(lt_evidence_set, rm_evidence_set, tg_evidence_set, ty_evidence_set,
                                       dg_evidence_set, dy_evidence_set)
        self.save_evidence_set(evidence_folder, evidence_set)

    def __collect_evidence_from_experiment_data(self, experiment_data_folder, experiment_id):
        number_of_rooms = len(data_adapter.ROOMS)
        number_of_green_victims = len(data_adapter.GREEN_VICTIMS)
        number_of_yellow_victims = len(data_adapter.YELLOW_VICTIMS)
        time_slices = data_adapter.MISSION_TIME_IN_SECONDS

        room_lit = [False] * number_of_rooms
        room_lit[0] = True  # Waiting Room
        room_lit[1] = True
        room_lit[3:12] = [True] * (12 - 3)  # These rooms are always lit

        observations = self.__read_observations(experiment_data_folder, experiment_id)
        light_events = self.__read_light_events(experiment_data_folder, experiment_id)
        triage_events = self.__read_triage_events(experiment_data_folder, experiment_id)

        most_recent_observation = observations[0]
        initial_time = most_recent_observation['data']['total_time']
        observation_counter = 0
        light_event_counter = 0
        triage_event_counter = 0

        lt_evidence = np.zeros(time_slices, dtype=np.int)
        rm_evidence = np.zeros(time_slices, dtype=np.int)
        tg_evidence = np.zeros(time_slices, dtype=np.int)
        ty_evidence = np.zeros(time_slices, dtype=np.int)
        dg_evidence = np.zeros((number_of_green_victims, time_slices))
        dy_evidence = np.zeros((number_of_yellow_victims, time_slices))

        for t in range(0, time_slices):  # Every one second until the end of the game
            # Get observation in the time slice
            while observations[observation_counter]['data'][
                'total_time'] <= initial_time + t * data_adapter.WORLD_TICK_PER_SECONDS:
                most_recent_observation = observations[observation_counter]
                observation_counter += 1

            # Get light event in the time slice
            while light_event_counter < len(light_events) and light_events[light_event_counter]['header'][
                'total_time'] <= initial_time + t * data_adapter.WORLD_TICK_PER_SECONDS:
                lever_x = light_events[light_event_counter]['data']['lever_x']
                lever_z = light_events[light_event_counter]['data']['lever_z']
                room_index = data_adapter.get_room_index_by_player_position(lever_x, lever_z)
                room_lit[room_index] = light_events[light_event_counter]['data']['powered']
                light_event_counter += 1

            # Get triage event in the time slice
            triaging_green = False
            triaging_yellow = False
            while triage_event_counter < len(triage_events) and triage_events[triage_event_counter]['header'][
                'total_time'] <= initial_time + t * data_adapter.WORLD_TICK_PER_SECONDS:
                if triage_events[triage_event_counter]['data']['color'] == 'Green':
                    triaging_green = True
                else:
                    triaging_yellow = True

                # victim_x = triage_events[triage_event_counter]['data']['victim_x']
                # victim_y = triage_events[triage_event_counter]['data']['victim_z']
                # if (victim_x, victim_y) in data_adapter.GREEN_VICTIMS.keys():
                #     triaging_green = True
                # elif (victim_x, victim_y) in data_adapter.YELLOW_VICTIMS.keys():
                #     triaging_yellow = True
                # triage_event_counter += 1

            player_x = most_recent_observation['data']['x']
            player_y = most_recent_observation['data']['z']
            room_index = data_adapter.get_room_index_by_player_position(player_x, player_y)

            lt_evidence[t] = 1 if room_lit[room_index] else 0
            rm_evidence[
                t] = room_index - 1  # Waiting Room is not accounted for. The player starts at the Staging Area in this implementation
            tg_evidence[t] = 1 if triaging_green else 0
            ty_evidence[t] = 1 if triaging_yellow else 0
            # distances = []
            for coordinates, v in data_adapter.GREEN_VICTIMS.items():
                victim_x = coordinates[0]
                victim_y = coordinates[1]
                dg_evidence[v][t] = self.calculate_distance(victim_x, victim_y, player_x, player_y)

            for coordinates, v in data_adapter.YELLOW_VICTIMS.items():
                victim_x = coordinates[0]
                victim_y = coordinates[1]
                dy_evidence[v][t] = self.calculate_distance(victim_x, victim_y, player_x, player_y)

        return lt_evidence, rm_evidence, tg_evidence, ty_evidence, dg_evidence, dy_evidence

    def __read_observations(self, experiment_data_folder, experiment_id):
        filepath = '{}/{}/{}'.format(experiment_data_folder, experiment_id,
                                     data_adapter.TestbedDataAdapter.OBSERVATIONS_FILENAME)
        return self.__read_data_from_file(filepath)

    def __read_light_events(self, experiment_data_folder, experiment_id):
        filepath = '{}/{}/{}'.format(experiment_data_folder, experiment_id,
                                     data_adapter.TestbedDataAdapter.LEVER_EVENT_FILENAME)
        return self.__read_data_from_file(filepath)

    def __read_room_events(self, experiment_data_folder, experiment_id):
        filepath = '{}/{}/{}'.format(experiment_data_folder, experiment_id,
                                     data_adapter.TestbedDataAdapter.ROOM_EVENT_FILENAME)
        return self.__read_data_from_file(filepath)

    def __read_triage_events(self, experiment_data_folder, experiment_id):
        filepath = '{}/{}/{}'.format(experiment_data_folder, experiment_id,
                                     data_adapter.TestbedDataAdapter.TRIAGE_EVENT_FILENAME)
        return self.__read_data_from_file(filepath)

    def __read_data_from_file(self, filepath):
        data = []
        with open(filepath, 'r') as file:
            for json_obj in file:
                data.append(json.loads(json_obj))

        return data

    def calculate_distance(self, victim_x, victim_y, player_x, player_y):
        return np.sqrt((victim_x - player_x) ** 2 + (victim_y - player_y) ** 2)

    def save_evidence_set(self, evidence_folder, evidence_set):
        np.save(self.get_evidence_filepath(evidence_folder, DataProcessing.LT_EVIDENCE_FILENAME),
                evidence_set.lt_evidence)
        np.save(self.get_evidence_filepath(evidence_folder, DataProcessing.LT_EVIDENCE_FILENAME),
                evidence_set.rm_evidence)
        np.save(self.get_evidence_filepath(evidence_folder, DataProcessing.LT_EVIDENCE_FILENAME),
                evidence_set.tg_evidence)
        np.save(self.get_evidence_filepath(evidence_folder, DataProcessing.LT_EVIDENCE_FILENAME),
                evidence_set.ty_evidence)
        np.save(self.get_evidence_filepath(evidence_folder, DataProcessing.LT_EVIDENCE_FILENAME),
                evidence_set.dg_evidence)
        np.save(self.get_evidence_filepath(evidence_folder, DataProcessing.LT_EVIDENCE_FILENAME),
                evidence_set.dy_evidence)

    def load_evidence_set(self, evidence_folder):
        lt_evidence_set = np.load(self.get_evidence_filepath(evidence_folder, DataProcessing.LT_EVIDENCE_FILENAME))
        rm_evidence_set = np.load(self.get_evidence_filepath(evidence_folder, DataProcessing.RM_EVIDENCE_FILENAME))
        tg_evidence_set = np.load(self.get_evidence_filepath(evidence_folder, DataProcessing.TG_EVIDENCE_FILENAME))
        ty_evidence_set = np.load(self.get_evidence_filepath(evidence_folder, DataProcessing.TY_EVIDENCE_FILENAME))
        dg_evidence_set = np.load(self.get_evidence_filepath(evidence_folder, DataProcessing.DG_EVIDENCE_FILENAME))
        dy_evidence_set = np.load(self.get_evidence_filepath(evidence_folder, DataProcessing.DY_EVIDENCE_FILENAME))

        return EvidenceDataSet(lt_evidence_set, rm_evidence_set, tg_evidence_set, ty_evidence_set, dg_evidence_set,
                               dy_evidence_set)

    def get_evidence_filepath(self, evidence_folder, evidence_filename):
        return '{}/{}.npy'.format(evidence_folder, evidence_filename)

    def save_theta_s(self, parameters_folder, theta_s, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.THETA_S_FILENAME, filename_suffix)
        np.save(filepath, theta_s)

    def save_pi_lt(self, parameters_folder, pi_lt, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.PI_LT_FILENAME, filename_suffix)
        np.save(filepath, pi_lt)

    def save_theta_rm(self, parameters_folder, theta_rm, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.THETA_RM_FILENAME, filename_suffix)
        np.save(filepath, theta_rm)

    def save_pi_tg(self, parameters_folder, pi_tg, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.PI_TG_FILENAME, filename_suffix)
        np.save(filepath, pi_tg)

    def save_pi_ty(self, parameters_folder, pi_ty, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.PI_TY_FILENAME, filename_suffix)
        np.save(filepath, pi_ty)

    def save_sigma_dg(self, parameters_folder, sigma_dg, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.SIGMA_DG_FILENAME, filename_suffix)
        np.save(filepath, sigma_dg)

    def save_sigma_dy(self, parameters_folder, sigma_dy, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.SIGMA_DY_FILENAME, filename_suffix)
        np.save(filepath, sigma_dy)

    def load_theta_s(self, parameters_folder, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.THETA_S_FILENAME, filename_suffix)
        return np.load(filepath)

    def load_pi_lt(self, parameters_folder, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.PI_LT_FILENAME, filename_suffix)
        return np.load(filepath)

    def load_theta_rm(self, parameters_folder, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.THETA_RM_FILENAME, filename_suffix)
        return np.load(filepath)

    def load_pi_tg(self, parameters_folder, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.PI_TG_FILENAME, filename_suffix)
        return np.load(filepath)

    def load_pi_ty(self, parameters_folder, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.PI_TY_FILENAME, filename_suffix)
        return np.load(filepath)

    def load_sigma_dg(self, parameters_folder, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.SIGMA_DG_FILENAME, filename_suffix)
        return np.load(filepath)

    def load_sigma_dy(self, parameters_folder, filename_suffix):
        filepath = self.get_parameter_filepath(parameters_folder, DataProcessing.SIGMA_DY_FILENAME, filename_suffix)
        return np.load(filepath)

    def get_parameter_filepath(self, parameters_folder, parameter_filename, filename_suffix):
        return '{}/{}_{}.npy'.format(parameters_folder, parameter_filename, filename_suffix)

    def save_predictions(self, predictions_folder, filename_suffix, lt_predictions, rm_predictions, tg_predictions,
                         ty_predictions, dg_predictions,
                         dy_predictions):
        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.LT_EVIDENCE_FILENAME,
                                                 filename_suffix)
        np.save(filepath, lt_predictions)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.RM_EVIDENCE_FILENAME,
                                                 filename_suffix)
        np.save(filepath, rm_predictions)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.TG_EVIDENCE_FILENAME,
                                                 filename_suffix)
        np.save(filepath, tg_predictions)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.TY_EVIDENCE_FILENAME,
                                                 filename_suffix)
        np.save(filepath, ty_predictions)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.DG_EVIDENCE_FILENAME,
                                                 filename_suffix)
        np.save(filepath, dg_predictions)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.DY_EVIDENCE_FILENAME,
                                                 filename_suffix)
        np.save(filepath, dy_predictions)

    def load_predictions(self, predictions_folder, filename_suffix):
        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.LT_EVIDENCE_FILENAME,
                                                 filename_suffix)
        lt_predictions = np.load(filepath)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.RM_EVIDENCE_FILENAME,
                                                 filename_suffix)
        rm_predictions = np.load(filepath)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.TG_EVIDENCE_FILENAME,
                                                 filename_suffix)
        tg_predictions = np.load(filepath)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.TY_EVIDENCE_FILENAME,
                                                 filename_suffix)
        ty_predictions = np.load(filepath)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.DG_EVIDENCE_FILENAME,
                                                 filename_suffix)
        dg_predictions = np.load(filepath)

        filepath = self.get_predictions_filepath(predictions_folder, DataProcessing.DY_EVIDENCE_FILENAME,
                                                 filename_suffix)
        dy_predictions = np.load(filepath)

        return lt_predictions, rm_predictions, tg_predictions, ty_predictions, dg_predictions, dy_predictions

    def get_predictions_filepath(self, predictions_folder, predictions_filename, filename_suffix):
        return '{}/{}_{}.npy'.format(predictions_folder, predictions_filename, filename_suffix)
