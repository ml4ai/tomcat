from distribution.discrete.multinomial import Multinomial
from distribution.discrete.binomial import Binomial
from distribution.continuous.beta import Beta
from distribution.continuous.dirichlet import Dirichlet
from distribution.continuous.gaussian import Gaussian
from distribution.continuous.inverse_gamma import InverseGamma
from base.node_metadata import NodeMetadata
from base.edge_metadata import EdgeMetadata
from distribution.cpd import CPD
from model.pgm_metadata import PGMMetadata
from base.node import Node
from model.pgm import PGM
from estimator.gibbs_estimator import GibbsEstimator
from sampling.ancestral_sampling import AncestralSampling
from adapter import data_adapter
import random
import numpy as np
import numpy as np
from tqdm import tqdm
import os
import pandas as pd
import json

NUMBER_OF_GREEN_VICTIMS = 1
NUMBER_OF_YELLOW_VICTIMS = 1
NUMBER_OF_STATES = 116
NUMBER_OF_ROOMS = len(data_adapter.ROOMS)
EPSILON = 0.0000001


class NodeLabel:
    S = 'S'
    LT = 'LT'
    RM = 'RM'
    TG = 'TG'
    TY = 'TY'
    DGv = 'DG{}'
    DYv = 'DY{}'

    # Parameter Nodes
    THETA_S_SX = 'THETA_S{}'
    PI_LT_SX = 'PI_LT_S{}'
    SIGMA_DGv_SX = 'SIGMA_DG{}_S{}'
    SIGMA_DYv_SX = 'SIGMA_DY{}_s{}'


class ModelDataBuilder:
    """
    This class extracts evidence data from testbed formatted data
    """

    def __init__(self, input_data_folder):
        self.input_data_folder = input_data_folder

    def collect_evidence_from_experiments(self):
        evidence_set = []
        for experiment_folder in tqdm(os.listdir(self.input_data_folder), desc='Retrieving evidence data'):
            evidence_set.append(self.__collect_evidence_from_experiment_data(experiment_folder))

        return pd.DataFrame(evidence_set)

    def __collect_evidence_from_experiment_data(self, experiment_folder):
        room_lit = [False] * NUMBER_OF_ROOMS
        room_lit[3:11] = [True] * (11 - 3)  # These rooms are always lit

        evidence = {}
        observations = self.__read_observations(experiment_folder)
        light_events = self.__read_light_events(experiment_folder)
        triage_events = self.__read_triage_events(experiment_folder)

        most_recent_observation = observations[0]
        initial_time = most_recent_observation['data']['total_time']
        observation_counter = 0
        light_event_counter = 0
        triage_event_counter = 0
        for t in range(0, data_adapter.MISSION_TIME_IN_SECONDS):  # Every one second until the end of the game
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
                victim_x = triage_events[triage_event_counter]['data']['victim_x']
                victim_y = triage_events[triage_event_counter]['data']['victim_z']
                if (victim_x, victim_y) in data_adapter.GREEN_VICTIMS.keys():
                    triaging_green = True
                elif (victim_x, victim_y) in data_adapter.YELLOW_VICTIMS.keys():
                    triaging_yellow = True
                triage_event_counter += 1

            player_x = most_recent_observation['data']['x']
            player_y = most_recent_observation['data']['z']
            room_index = data_adapter.get_room_index_by_player_position(player_x, player_y)

            evidence[(NodeLabel.LT, t)] = 1 if room_lit[room_index] else 0
            evidence[(NodeLabel.RM, t)] = room_index
            evidence[(NodeLabel.TG, t)] = 1 if triaging_green else 0
            evidence[(NodeLabel.TY, t)] = 1 if triaging_yellow else 0
            distances = []
            for coordinates, index in data_adapter.GREEN_VICTIMS.items():
                victim_x = coordinates[0]
                victim_y = coordinates[1]
                distances.append(self.get_distance(victim_x, victim_y, player_x, player_y))
                # evidence[(NodeLabel.DGv.format(index), t)] = self.get_distance(victim_x, victim_y, player_x,
                #                                                                player_y)
            evidence[(NodeLabel.DGv.format(0), t)] = np.min(distances)
            for coordinates, index in data_adapter.YELLOW_VICTIMS.items():
                victim_x = coordinates[0]
                victim_y = coordinates[1]
                distances.append(self.get_distance(victim_x, victim_y, player_x, player_y))
                evidence[(NodeLabel.DYv.format(index), t)] = self.get_distance(victim_x, victim_y, player_x,
                                                                               player_y)
            evidence[(NodeLabel.DYv.format(0), t)] = np.min(distances)

        return evidence

    def __read_observations(self, experiment_folder):
        filepath = '{}/{}/{}'.format(self.input_data_folder, experiment_folder,
                                     data_adapter.TestbedDataAdapter.OBSERVATIONS_FILENAME)
        return self.__read_data_from_file(filepath)

    def __read_light_events(self, experiment_folder):
        filepath = '{}/{}/{}'.format(self.input_data_folder, experiment_folder,
                                     data_adapter.TestbedDataAdapter.LEVER_EVENT_FILENAME)
        return self.__read_data_from_file(filepath)

    def __read_room_events(self, experiment_folder):
        filepath = '{}/{}/{}'.format(self.input_data_folder, experiment_folder,
                                     data_adapter.TestbedDataAdapter.ROOM_EVENT_FILENAME)
        return self.__read_data_from_file(filepath)

    def __read_triage_events(self, experiment_folder):
        filepath = '{}/{}/{}'.format(self.input_data_folder, experiment_folder,
                                     data_adapter.TestbedDataAdapter.TRIAGE_EVENT_FILENAME)
        return self.__read_data_from_file(filepath)

    def __read_data_from_file(self, filepath):
        data = []
        with open(filepath, 'r') as file:
            for json_obj in file:
                data.append(json.loads(json_obj))

        return data

    def get_distance(self, victim_x, victim_y, player_x, player_y):
        return np.sqrt((victim_x - player_x) ** 2 + (victim_y - player_y) ** 2)


class ModelBuilder():
    def create_nodes_metadata(self):
        nodes_metadata = {}

        nodes_metadata[NodeLabel.S] = NodeMetadata(NodeLabel.S, cardinality=NUMBER_OF_STATES, repeatable=True)
        nodes_metadata[NodeLabel.LT] = NodeMetadata(NodeLabel.LT, cardinality=2, repeatable=True, first_time_slice=1)
        nodes_metadata[NodeLabel.RM] = NodeMetadata(NodeLabel.RM, cardinality=NUMBER_OF_ROOMS, repeatable=True,
                                                    first_time_slice=1)
        nodes_metadata[NodeLabel.TG] = NodeMetadata(NodeLabel.TG, cardinality=2, repeatable=True, first_time_slice=1)
        nodes_metadata[NodeLabel.TY] = NodeMetadata(NodeLabel.TY, cardinality=2, repeatable=True, first_time_slice=1)

        for v in range(NUMBER_OF_GREEN_VICTIMS):
            distance_node_label = NodeLabel.DGv.format(v)
            nodes_metadata[distance_node_label] = NodeMetadata(distance_node_label, repeatable=True, first_time_slice=1)

        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            distance_node_label = NodeLabel.DYv.format(v)
            nodes_metadata[distance_node_label] = NodeMetadata(distance_node_label, repeatable=True, first_time_slice=1)

        return nodes_metadata

    def create_edges_from(self, nodes_metadata):
        edges = []
        state_nm = nodes_metadata[NodeLabel.S]
        edges.append(EdgeMetadata(state_nm, state_nm, forward_in_time=True))
        edges.append(EdgeMetadata(state_nm, nodes_metadata[NodeLabel.LT]))
        edges.append(EdgeMetadata(state_nm, nodes_metadata[NodeLabel.RM]))
        edges.append(EdgeMetadata(state_nm, nodes_metadata[NodeLabel.TG]))
        edges.append(EdgeMetadata(state_nm, nodes_metadata[NodeLabel.TY]))
        for v in range(NUMBER_OF_GREEN_VICTIMS):
            edges.append(EdgeMetadata(state_nm, nodes_metadata[NodeLabel.DGv.format(v)]))
        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            edges.append(EdgeMetadata(state_nm, nodes_metadata[NodeLabel.DYv.format(v)]))

        return edges

    def create_parameter_nodes_metadata(self):
        nodes_metadata = {}
        for s in range(NUMBER_OF_STATES):
            nodes_metadata[NodeLabel.THETA_S_SX.format(s)] = NodeMetadata(NodeLabel.THETA_S_SX.format(s),
                                                                          parameter=True,
                                                                          constant=True, first_time_slice=1)
            nodes_metadata[NodeLabel.PI_LT_SX.format(s)] = NodeMetadata(NodeLabel.PI_LT_SX.format(s), parameter=True,
                                                                        constant=True, first_time_slice=1)
            for v in range(NUMBER_OF_GREEN_VICTIMS):
                nodes_metadata[NodeLabel.SIGMA_DGv_SX.format(v, s)] = NodeMetadata(NodeLabel.SIGMA_DGv_SX.format(v, s),
                                                                                   parameter=True, constant=True,
                                                                                   first_time_slice=1)

            for v in range(NUMBER_OF_YELLOW_VICTIMS):
                nodes_metadata[NodeLabel.SIGMA_DYv_SX.format(v, s)] = NodeMetadata(NodeLabel.SIGMA_DYv_SX.format(v, s),
                                                                                   parameter=True, constant=True,
                                                                                   first_time_slice=1)
        return nodes_metadata

    def create_edges_from_parameter_nodes(self, parameter_nodes_metadata, nodes_metadata):
        edges = []
        for s in range(NUMBER_OF_STATES):
            edges.append(
                EdgeMetadata(parameter_nodes_metadata[NodeLabel.THETA_S_SX.format(s)], nodes_metadata[NodeLabel.S]))
            edges.append(
                EdgeMetadata(parameter_nodes_metadata[NodeLabel.PI_LT_SX.format(s)], nodes_metadata[NodeLabel.LT]))

            for v in range(NUMBER_OF_GREEN_VICTIMS):
                edges.append(EdgeMetadata(parameter_nodes_metadata[NodeLabel.SIGMA_DGv_SX.format(v, s)],
                                          nodes_metadata[NodeLabel.DGv.format(v)]))

            for v in range(NUMBER_OF_YELLOW_VICTIMS):
                edges.append(EdgeMetadata(parameter_nodes_metadata[NodeLabel.SIGMA_DYv_SX.format(v, s)],
                                          nodes_metadata[NodeLabel.DYv.format(v)]))

        return edges

    def get_one_hot_vector(self, size, one_indices, zero=EPSILON):
        vector = EPSILON * np.ones(size)
        for index in one_indices:
            vector[index] = 1

        return vector

    def get_theta_s_prior_per_hallway(self, state_number, adjacent_rooms_rw_state_numbers,
                                      adjacent_hallways_state_numbers):
        priors = {}
        valid_transitions = [state_number] + adjacent_rooms_rw_state_numbers + adjacent_hallways_state_numbers
        priors[NodeLabel.THETA_S_SX.format(state_number)] = Dirichlet(
            self.get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

        return priors

    def get_theta_s_prior_per_room(self, initial_state_number, adjacent_rooms_rw_state_numbers,
                                   adjacent_hallways_state_numbers):
        priors = {}

        # LRW to TGVLR, TYVLR, DRW | LRW, DRW adjacent rooms | HW adjacent Hallways
        valid_transitions = [range(initial_state_number,
                                   initial_state_number + 4)] + adjacent_rooms_rw_state_numbers + adjacent_hallways_state_numbers
        priors[NodeLabel.THETA_S_SX.format(initial_state_number)] = Dirichlet(
            self.get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number, initial_state_number + 1]
        priors[NodeLabel.THETA_S_SX.format(initial_state_number + 1)] = Dirichlet(
            self.get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number, initial_state_number + 2]
        priors[NodeLabel.THETA_S_SX.format(initial_state_number + 2)] = Dirichlet(
            self.get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

        # DRW to TGVDR, TYVDR, LRW | LRW, DRW adjacent rooms | HW adjacent Hallways
        valid_transitions = [initial_state_number, range(initial_state_number + 3,
                                                         initial_state_number + 6)] + adjacent_rooms_rw_state_numbers + adjacent_hallways_state_numbers
        priors[NodeLabel.THETA_S_SX.format(initial_state_number + 3)] = Dirichlet(
            self.get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number + 3, initial_state_number + 4]
        priors[NodeLabel.THETA_S_SX.format(initial_state_number + 4)] = Dirichlet(
            self.get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

        # Keep triaging + back to room walk (same room)
        valid_transitions = [initial_state_number + 3, initial_state_number + 5]
        priors[NodeLabel.THETA_S_SX.format(initial_state_number + 5)] = Dirichlet(
            self.get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

        return priors

    def get_theta_s_priors(self):
        priors = {}

        # P(theta_s | s_t-1 = HW_some_area)
        priors.update(self.get_theta_s_prior_per_hallway(0, [], [1]))
        priors.update(self.get_theta_s_prior_per_hallway(1, [], [2]))
        priors.update(self.get_theta_s_prior_per_hallway(2, [range(8, 32, 3)], [1, 3]))
        priors.update(self.get_theta_s_prior_per_hallway(3, [range(56, 98, 3)], [2, 4, 5]))
        priors.update(
            self.get_theta_s_prior_per_hallway(4, [range(14, 20, 3), range(32, 38, 3), range(56, 62, 3)], [2, 6]))
        priors.update(self.get_theta_s_prior_per_hallway(5, [range(74, 80, 3), range(98, 104, 3)], [2, 7]))
        priors.update(self.get_theta_s_prior_per_hallway(6, [range(14, 20, 3), range(32, 56, 3)], [4]))
        priors.update(self.get_theta_s_prior_per_hallway(7, [range(98, 116, 3)], [5]))

        # P(theta_s | s_t-1 = state_some_room)
        priors.update(self.get_theta_s_prior_per_room(8, [range(14, 20, 3)], [1]))
        priors.update(self.get_theta_s_prior_per_room(14, [range(8, 14, 3)], [1, 4, 6]))
        priors.update(self.get_theta_s_prior_per_room(20, [range(26, 32, 3)], [1]))
        priors.update(self.get_theta_s_prior_per_room(26, [range(20, 26, 3)], [1]))
        priors.update(self.get_theta_s_prior_per_room(32, [range(38, 44, 3), range(56, 68, 3)], [4, 6]))
        priors.update(self.get_theta_s_prior_per_room(38, [range(32, 38, 3), range(44, 50, 3), range(62, 68, 3)], [6]))
        priors.update(self.get_theta_s_prior_per_room(44, [range(38, 44, 3), range(50, 56, 3), range(62, 68, 3)], [6]))
        priors.update(self.get_theta_s_prior_per_room(50, [range(44, 50, 3), range(68, 74, 3)], [6]))
        priors.update(self.get_theta_s_prior_per_room(56, [range(32, 38, 3), range(62, 68, 3)], [3, 4]))
        priors.update(self.get_theta_s_prior_per_room(62, [range(32, 50, 3), range(56, 62, 3), range(68, 74, 3)], [3]))
        priors.update(self.get_theta_s_prior_per_room(68, [range(50, 56, 3), range(62, 68, 3)], [3]))
        priors.update(self.get_theta_s_prior_per_room(74, [range(80, 86, 3), range(98, 104, 3)], [3, 5]))
        priors.update(self.get_theta_s_prior_per_room(80, [range(74, 80, 3), range(86, 92, 3), range(98, 104, 3)], [3]))
        priors.update(
            self.get_theta_s_prior_per_room(86, [range(80, 86, 3), range(92, 98, 3), range(104, 110, 3)], [3]))
        priors.update(self.get_theta_s_prior_per_room(92, [range(86, 92, 3), range(110, 116, 3)], [3]))
        priors.update(self.get_theta_s_prior_per_room(98, [range(74, 86, 3), range(104, 110, 3)], [5, 7]))
        priors.update(
            self.get_theta_s_prior_per_room(104, [range(86, 92, 3), range(98, 104, 3), range(110, 116, 3)], [7]))
        priors.update(self.get_theta_s_prior_per_room(110, [range(92, 98, 3), range(104, 110, 3)], [7]))

        return priors

    def get_pi_lt_priors(self):
        priors = {}

        # Lights in the hallway states are always on
        for s in range(8):
            priors[NodeLabel.PI_LT_SX.format(s)] = Beta(EPSILON, 1)

        for s in range(8, 116):
            if int((s - 8) / 3) % 2 == 0:
                # States which the light is on
                priors[NodeLabel.PI_LT_SX.format(s)] = Beta(1, EPSILON)
            else:
                # States which the light is off
                priors[NodeLabel.PI_LT_SX.format(s)] = Beta(EPSILON, 1)

        return priors

    def get_rooms_distribution_per_state(self):
        distributions = []

        for s in range(8):
            distributions.append(Multinomial(self.get_one_hot_vector(NUMBER_OF_ROOMS, [s], zero=0)))

        for s in range(8, 116):
            room_index = int((s - 8) / 6) + 8
            distributions.append(Multinomial(self.get_one_hot_vector(NUMBER_OF_ROOMS, [room_index], zero=0)))

        return distributions

    def get_tg_distribution_per_state(self):
        distributions = []

        # No triaging in a hallway
        for s in range(8):
            distributions.append(Binomial(0))

        for s in range(8, NUMBER_OF_STATES):
            # P(TG|State) = 1 if State = Triaging Green, 0 otherwise
            pi = 1 if int((s - 9) % 6) == 0 else 0
            distributions.append(Binomial(pi))

        return distributions

    def get_ty_distribution_per_state(self):
        distributions = []

        # No triaging in a hallway
        for s in range(8):
            distributions.append(Binomial(0))

        for s in range(8, NUMBER_OF_STATES):
            # P(TY|State) = 1 if State = Triaging Yellow, 0 otherwise
            pi = 1 if int((s - 10) % 6) == 0 else 0
            distributions.append(Binomial(pi))

        return distributions

    def create_parameterized_cpds_for_individual_nodes(self, nodes_metadata, parameter_nodes_metadata):
        cpds = []
        state_nm = nodes_metadata[NodeLabel.S]
        state_parameter_parents = []
        light_parameter_parents = []
        state_parameterized_distributions = []
        light_parameterized_distributions = []

        theta_s_priors = self.get_theta_s_priors()
        pi_lt_priors = self.get_pi_lt_priors()

        for s in range(NUMBER_OF_STATES):
            parameter_parent_nm = parameter_nodes_metadata[NodeLabel.THETA_S_SX.format(s)]
            state_parameter_parents.append(parameter_parent_nm)
            state_parameterized_distributions.append(Multinomial(Node(parameter_parent_nm)))
            cpds.append(CPD(parameter_parent_nm, [], theta_s_priors[NodeLabel.THETA_S_SX.format(s)]))

            parameter_parent_nm = parameter_nodes_metadata[NodeLabel.PI_LT_SX.format(s)]
            light_parameter_parents.append(parameter_parent_nm)
            light_parameterized_distributions.append(Binomial(Node(parameter_parent_nm)))
            cpds.append(CPD(parameter_parent_nm, [], pi_lt_priors[NodeLabel.PI_LT_SX.format(s)]))

        cpds.append(CPD(state_nm, [], Multinomial(
            self.get_one_hot_vector(NUMBER_OF_STATES, [1], zero=0))))  # The first state is always HW_StatingArea
        cpds.append(CPD(state_nm, [state_nm] + state_parameter_parents, state_parameterized_distributions))
        cpds.append(
            CPD(nodes_metadata[NodeLabel.LT], [state_nm] + light_parameter_parents, light_parameterized_distributions))
        cpds.append(CPD(nodes_metadata[NodeLabel.TG], [state_nm], self.get_tg_distribution_per_state()))
        cpds.append(CPD(nodes_metadata[NodeLabel.TY], [state_nm], self.get_ty_distribution_per_state()))
        cpds.append(CPD(nodes_metadata[NodeLabel.RM], [state_nm], self.get_rooms_distribution_per_state()))

        return cpds

    def create_parameterized_cpds_for_multinodes(self, nodes_metadata, parameter_nodes_metadata):
        cpds = []
        state_nm = nodes_metadata[NodeLabel.S]

        # Parameter nodes for individual victims given a state
        for v in range(NUMBER_OF_GREEN_VICTIMS):
            distance_green_parameter_parents = []
            distance_green_parameterized_distributions = []
            for s in range(NUMBER_OF_STATES):
                parameter_parent_nm = parameter_nodes_metadata[NodeLabel.SIGMA_DGv_SX.format(v, s)]
                distance_green_parameter_parents.append(parameter_parent_nm)
                distance_green_parameterized_distributions.append(Gaussian(0, Node(parameter_parent_nm)))
                cpds.append(CPD(parameter_parent_nm, [], InverseGamma(1, 1)))  # Fixed prior

            cpds.append(CPD(nodes_metadata[NodeLabel.DGv.format(v)], [state_nm] + distance_green_parameter_parents,
                            distance_green_parameterized_distributions))

        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            distance_yellow_parameter_parents = []
            distance_yellow_parameterized_distributions = []
            for s in range(NUMBER_OF_STATES):
                parameter_parent_nm = parameter_nodes_metadata[NodeLabel.SIGMA_DYv_SX.format(v, s)]
                distance_yellow_parameter_parents.append(parameter_parent_nm)
                distance_yellow_parameterized_distributions.append(Gaussian(0, Node(parameter_parent_nm)))
                cpds.append(CPD(parameter_parent_nm, [], InverseGamma(1, 1)))  # Fixed prior

            cpds.append(CPD(nodes_metadata[NodeLabel.DYv.format(v)], [state_nm] + distance_yellow_parameter_parents,
                            distance_yellow_parameterized_distributions))

        return cpds

    def create_parameterized_cpds(self, nodes_metadata, parameter_nodes_metadata):
        cpds = self.create_parameterized_cpds_for_individual_nodes(nodes_metadata, parameter_nodes_metadata)
        cpds += self.create_parameterized_cpds_for_multinodes(nodes_metadata, parameter_nodes_metadata)
        return cpds

    def build_pgm(self, time_slices):
        metadata = PGMMetadata()
        nodes_metadata = self.create_nodes_metadata()
        parameter_nodes_metadata = self.create_parameter_nodes_metadata()

        metadata.add_nodes_from(nodes_metadata.values())
        metadata.add_nodes_from(parameter_nodes_metadata.values())
        metadata.add_edges_from(self.create_edges_from(nodes_metadata))
        metadata.add_edges_from(self.create_edges_from_parameter_nodes(parameter_nodes_metadata, nodes_metadata))
        metadata.add_cpds_from(self.create_parameterized_cpds(nodes_metadata, parameter_nodes_metadata))

        pgm = PGM(metadata, time_slices)
        return pgm


class TA3ParameterEstimation(GibbsEstimator):

    def sample_parameters(self, data, number_of_samples=500, burn_in_periods=100):
        if data.empty:
            raise TypeError('Data is mandatory for parameter estimation')

        samples = []

        # The following procedure will complete the data with samples for
        # the latent variables using ancestral sampling
        parameter_sample, data_and_samples = self.get_initial_estimates(data)

        for _ in tqdm(range(burn_in_periods), desc="Burn-in"):
            parameter_sample, data_and_samples = self.get_single_sample_estimate(data_and_samples, parameter_sample)

        for _ in tqdm(range(number_of_samples), desc="Samples"):
            parameter_sample, data_and_samples = self.get_single_sample_estimate(data_and_samples, parameter_sample)
            samples.append(parameter_sample)

        return pd.DataFrame(samples)

    def get_initial_estimates(self, data):
        """
        This function completes the data with sampled values for each latent variable in each data point.
        Constant nodes will preserve the value sampled in the first data point
        """
        sampling = AncestralSampling(self.pgm)
        samples = []

        copied_data = data.copy()
        number_of_data_points = len(data.index)

        # Constant nodes will have a constant value for all the sampled data
        parameter_sample = sampling.sample()
        parameter_sample = parameter_sample.iloc[0]
        parameter_sample = parameter_sample[self.pgm.get_parameter_nodes_id()]  # Sample contains only values assigned to the parameter nodes
        self.assign_values_to_nodes(parameter_sample)

        # Sample S for each data_point
        sampled_data_points = []
        for _, data_point in tqdm(copied_data.iterrows(), desc='Initial sample'):
            data_point_dict = data_point.to_dict()
            past_s = 1
            data_point_dict[(NodeLabel.S, 0)] = int(past_s)
            for t in range(1, self.pgm.time_slices):
                node_s = self.pgm.get_node((NodeLabel.S, t))
                distribution = node_s.cpd.get_distributions()[past_s]
                past_s = distribution.sample()
                data_point_dict[node_s.get_id()] = int(past_s)

            sampled_data_points.append(data_point_dict)

        return parameter_sample, pd.DataFrame(sampled_data_points)

    def get_single_sample_estimate(self, data_and_samples, parameter_sample):
        # Store some computations to accelerate finding the posterior for each node

        count_states_future_given_past = [[0 for _ in range(NUMBER_OF_STATES)] for _ in range(NUMBER_OF_STATES)] * NUMBER_OF_STATES  # [s_t][s_t+1][freq0, freq1, ..., freq115]
        green_victims_distance_by_state = [[[] for _ in range(NUMBER_OF_STATES)] for _ in range(NUMBER_OF_GREEN_VICTIMS)]  # [v][state][d0, d1, ..., d115]
        yellow_victims_distance_by_state = [[[] for _ in range(NUMBER_OF_STATES)] for _ in range(NUMBER_OF_YELLOW_VICTIMS)]  # [v][state][d0, d1, ..., d115]
        children_distributions_given_state_per_time = [{} for _ in range(self.pgm.time_slices)]  # A list of dictionary of child nodes and
        # their distributions per time [t][child_node][dist|s_t = 0, dist|s_t = 1..., dist|s_t = 115]
        # LT, RM, TG, TY, DGv, DYv

        for t in range(self.pgm.time_slices):
            if t < self.pgm.time_slices - 1:
                for _, data_point in data_and_samples.iterrows():
                    current_state = int(data_point[(NodeLabel.S, t)])
                    future_state = int(data_point[(NodeLabel.S, t + 1)])
                    count_states_future_given_past[current_state][future_state] += 1

                    if t > 0:
                        for v in range(NUMBER_OF_GREEN_VICTIMS):
                            distance = data_point[(NodeLabel.DGv.format(v), t)]
                            green_victims_distance_by_state[v][current_state].append(distance)

                        for v in range(NUMBER_OF_YELLOW_VICTIMS):
                            distance = data_point[(NodeLabel.DYv.format(v), t)]
                            yellow_victims_distance_by_state[v][current_state].append(distance)

            if t > 0:
                if t < self.pgm.time_slices - 1:
                    children_distributions_given_state_per_time[t][NodeLabel.S] = self.pgm.get_node(
                        (NodeLabel.S, t+1)).cpd.get_distributions()

                children_distributions_given_state_per_time[t][NodeLabel.LT] = self.pgm.get_node(
                    (NodeLabel.LT, t)).cpd.get_distributions()
                children_distributions_given_state_per_time[t][NodeLabel.RM] = self.pgm.get_node(
                    (NodeLabel.RM, t)).cpd.get_distributions()
                children_distributions_given_state_per_time[t][NodeLabel.TG] = self.pgm.get_node(
                    (NodeLabel.TG, t)).cpd.get_distributions()
                children_distributions_given_state_per_time[t][NodeLabel.TY] = self.pgm.get_node(
                    (NodeLabel.TY, t)).cpd.get_distributions()

                for v in range(NUMBER_OF_GREEN_VICTIMS):
                    children_distributions_given_state_per_time[t][NodeLabel.DGv.format(v)] = self.pgm.get_node(
                        (NodeLabel.DGv.format(v), t)).cpd.get_distributions()

                for v in range(NUMBER_OF_YELLOW_VICTIMS):
                    children_distributions_given_state_per_time[t][NodeLabel.DYv.format(v)] = self.pgm.get_node(
                        (NodeLabel.DYv.format(v), t)).cpd.get_distributions()

        # Sample parameter nodes first
        for s in range(NUMBER_OF_STATES):
            # THETA_S
            theta_s_node = self.pgm.get_node((NodeLabel.THETA_S_SX.format(s), 1))
            prior = theta_s_node.cpd.get_distribution()[0]
            posterior_theta_s = prior.conjugate(Multinomial([]), count_states_future_given_past[s])
            sampled_value = posterior_theta_s.sample()
            parameter_sample[theta_s_node.get_id()] = sampled_value
            theta_s_node.assignment = sampled_value  # Fill the node so when sampling S
            # we have the last sampled parameter value

            # SIGMA_DG
            for v in range(NUMBER_OF_GREEN_VICTIMS):
                sigma_s_node = self.pgm.get_node((NodeLabel.SIGMA_DGv_SX.format(v, s), 1))
                prior = sigma_s_node.cpd.get_distribution()[0]
                posterior_sigma_s = prior.conjugate(Gaussian(0,1), green_victims_distance_by_state[v][s])
                sampled_value = posterior_sigma_s.sample()
                parameter_sample[sigma_s_node.get_id()] = sampled_value
                sigma_s_node.assignment = sampled_value

            # SIGMA_DY
            for v in range(NUMBER_OF_YELLOW_VICTIMS):
                sigma_s_node = self.pgm.get_node((NodeLabel.SIGMA_DYv_SX.format(v, s), 1))
                prior = sigma_s_node.cpd.get_distribution()[0]
                posterior_sigma_s = prior.conjugate(Gaussian(0,1), yellow_victims_distance_by_state[v][s])
                sampled_value = posterior_sigma_s.sample()
                parameter_sample[sigma_s_node.get_id()] = sampled_value
                sigma_s_node.assignment = sampled_value

        # Sample State
        for t in tqdm(range(1, self.pgm.time_slices), desc='Sampling S'):
            node_s = self.pgm.get_node((NodeLabel.S, t))
            parents_ids = self.pgm.get_parent_nodes_id_of(node_s)

            assignments = []
            for _, data_point in data_and_samples.iterrows():
                evidence_from_parents = self.remove_time_slice_indicator(data_point[parents_ids])
                prior = node_s.cpd.get_distribution(evidence_from_parents)[0]

                density_per_state = [0] * 116
                for s in range(NUMBER_OF_STATES):
                    if t < self.pgm.time_slices - 1:
                        value = int(data_point[(NodeLabel.S, t+1)])
                        distribution = children_distributions_given_state_per_time[t][NodeLabel.S][s]
                        temp = pd.Series({value: 1})
                        density_per_state[s] += distribution.get_probability(temp, log_transform=True)

                    value = int(data_point[(NodeLabel.LT, t)])
                    distribution = children_distributions_given_state_per_time[t][NodeLabel.LT][s]
                    density_per_state[s] += distribution.get_probability(pd.Series({value: 1}), log_transform=True)

                    value = int(data_point[(NodeLabel.RM, t)])
                    distribution = children_distributions_given_state_per_time[t][NodeLabel.RM][s]
                    density_per_state[s] += distribution.get_probability(pd.Series({value: 1}), log_transform=True)

                    value = int(data_point[(NodeLabel.TG, t)])
                    distribution = children_distributions_given_state_per_time[t][NodeLabel.TG][s]
                    density_per_state[s] += distribution.get_probability(pd.Series({value: 1}), log_transform=True)

                    value = int(data_point[(NodeLabel.TY, t)])
                    distribution = children_distributions_given_state_per_time[t][NodeLabel.TY][s]
                    density_per_state[s] += distribution.get_probability(pd.Series({value: 1}), log_transform=True)

                    for v in range(NUMBER_OF_GREEN_VICTIMS):
                        value = data_point[(NodeLabel.DGv.format(v), t)]
                        distribution = children_distributions_given_state_per_time[t][NodeLabel.DGv.format(v)][s]
                        density_per_state[s] += distribution.get_probability(pd.Series({value: 1}), log_transform=True)

                    for v in range(NUMBER_OF_YELLOW_VICTIMS):
                        value = data_point[(NodeLabel.DYv.format(v), t)]
                        distribution = children_distributions_given_state_per_time[t][NodeLabel.DYv.format(v)][s]
                        density_per_state[s] += distribution.get_probability(pd.Series({value: 1}), log_transform=True)

                posterior_s = prior.mult(density_per_state, in_log_scale=True)
                sampled_value = posterior_s.sample()
                assignments.append(sampled_value)

            data_and_samples[node_s.get_id()] = pd.Series(assignments)

        return parameter_sample, data_and_samples

class ExperimentRunner():

    def __init__(self):
        self.model_builder = ModelBuilder()

    def generate_synthetic_data(self, number_of_data_points, output_filepath):
        print("Generating synthetic data...")
        self.reset_random_seed()
        pgm = self.model_builder.build_pgm(data_adapter.MISSION_TIME_IN_SECONDS)
        sampling = AncestralSampling(pgm)
        data_points = sampling.sample(number_of_data_points)
        data_points.to_csv(output_filepath)

    def estimate_parameters(self, data_filepath, output_filepath, burn_in, number_of_samples):
        print("Estimating parameters from evidence data...")
        self.reset_random_seed()
        data = data_adapter.read_evidence_data_from_file(data_filepath)
        pgm = self.model_builder.build_pgm(data_adapter.MISSION_TIME_IN_SECONDS)
        estimator = TA3ParameterEstimation(pgm)
        parameters = estimator.estimate_parameters_from(data, burn_in, number_of_samples)
        parameters.to_csv(output_filepath)

    def reset_random_seed(self):
        random.seed(42)
        np.random.seed(42)


if __name__ == '__main__':
    TESTBED_DATA_FILEPATH = '../data/input/testbed'
    EVIDENCE_DATA_FILEPATH = '../data/input/evidence/sar_internal_data_collection.csv'

    model_data_builder = ModelDataBuilder(TESTBED_DATA_FILEPATH)
    evidence_set = model_data_builder.collect_evidence_from_experiments()
    evidence_set.to_csv(EVIDENCE_DATA_FILEPATH)

    PARAMETERS_FILEPATH = '../data/output/parameter_estimation/tomcat_ta3'
    NUMBER_OF_SAMPLES = 1000
    BURN_IN = 100

    experiment_runner = ExperimentRunner()
    experiment_runner.estimate_parameters(EVIDENCE_DATA_FILEPATH, PARAMETERS_FILEPATH, BURN_IN, NUMBER_OF_SAMPLES)
