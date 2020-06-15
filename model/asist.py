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
import numpy as np
import adaptor.tomcat_data_adapter as data_adapter
import pandas as pd
from tqdm import tqdm
import os
import json

NUMBER_OF_GREEN_VICTIMS = 10;
NUMBER_OF_YELLOW_VICTIMS = 10;
NUMBER_OF_STATES = 116
NUMBER_OF_ROOMS = 26
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
    PI_TG_SX = 'PI_TG_S{}'
    PI_TY_SX = 'PI_TY_S{}'
    SIGMA_DGv_SX = 'SIGMA_DG{}_S{}'
    SIGMA_DYv_SX = 'SIGMA_DY{}_s{}'


def create_nodes_metadata():
    nodes_metadata = {}

    nodes_metadata[NodeLabel.S] = NodeMetadata(NodeLabel.S, cardinality=NUMBER_OF_STATES, repeatable=True)
    nodes_metadata[NodeLabel.LT] = NodeMetadata(NodeLabel.LT, cardinality=3, repeatable=True, first_time_slice=1)
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


def create_edges_from(nodes_metadata):
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


def create_parameter_nodes_metadata():
    nodes_metadata = {}
    for s in range(NUMBER_OF_STATES):
        nodes_metadata[NodeLabel.THETA_S_SX.format(s)] = NodeMetadata(NodeLabel.THETA_S_SX.format(s), parameter=True,
                                                                      constant=True, first_time_slice=1)
        nodes_metadata[NodeLabel.PI_LT_SX.format(s)] = NodeMetadata(NodeLabel.PI_LT_SX.format(s), parameter=True,
                                                                    constant=True, first_time_slice=1)
        nodes_metadata[NodeLabel.PI_TG_SX.format(s)] = NodeMetadata(NodeLabel.PI_TG_SX.format(s), parameter=True,
                                                                    constant=True, first_time_slice=1)
        nodes_metadata[NodeLabel.PI_TY_SX.format(s)] = NodeMetadata(NodeLabel.PI_TY_SX.format(s), parameter=True,
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


def create_edges_from_parameter_nodes(parameter_nodes_metadata, nodes_metadata):
    edges = []
    for s in range(NUMBER_OF_STATES):
        edges.append(
            EdgeMetadata(parameter_nodes_metadata[NodeLabel.THETA_S_SX.format(s)], nodes_metadata[NodeLabel.S]))
        edges.append(
            EdgeMetadata(parameter_nodes_metadata[NodeLabel.PI_LT_SX.format(s)], nodes_metadata[NodeLabel.LT]))
        edges.append(
            EdgeMetadata(parameter_nodes_metadata[NodeLabel.PI_TG_SX.format(s)], nodes_metadata[NodeLabel.TG]))
        edges.append(
            EdgeMetadata(parameter_nodes_metadata[NodeLabel.PI_TY_SX.format(s)], nodes_metadata[NodeLabel.TY]))

        for v in range(NUMBER_OF_GREEN_VICTIMS):
            edges.append(EdgeMetadata(parameter_nodes_metadata[NodeLabel.SIGMA_DGv_SX.format(v, s)],
                                      nodes_metadata[NodeLabel.DGv.format(v)]))

        for v in range(NUMBER_OF_YELLOW_VICTIMS):
            edges.append(EdgeMetadata(parameter_nodes_metadata[NodeLabel.SIGMA_DYv_SX.format(v, s)],
                                      nodes_metadata[NodeLabel.DYv.format(v)]))

    return edges


def get_one_hot_vector(size, one_indices, zero=EPSILON):
    vector = EPSILON * np.ones(size)
    for index in one_indices:
        vector[index] = 1

    return vector


def get_theta_s_prior_per_hallway(state_number, adjacent_rooms_rw_state_numbers, adjacent_hallways_state_numbers):
    priors = {}
    valid_transitions = [state_number] + adjacent_rooms_rw_state_numbers + adjacent_hallways_state_numbers
    priors[NodeLabel.THETA_S_SX.format(state_number)] = Dirichlet(
        get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

    return priors


def get_theta_s_prior_per_room(initial_state_number, adjacent_rooms_rw_state_numbers, adjacent_hallways_state_numbers):
    priors = {}

    # LRW to TGVLR, TYVLR, DRW | LRW, DRW adjacent rooms | HW adjacent Hallways
    valid_transitions = [range(initial_state_number,
                               initial_state_number + 4)] + adjacent_rooms_rw_state_numbers + adjacent_hallways_state_numbers
    priors[NodeLabel.THETA_S_SX.format(initial_state_number)] = Dirichlet(
        get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

    # Keep triaging + back to room walk (same room)
    valid_transitions = [initial_state_number, initial_state_number + 1]
    priors[NodeLabel.THETA_S_SX.format(initial_state_number + 1)] = Dirichlet(
        get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

    # Keep triaging + back to room walk (same room)
    valid_transitions = [initial_state_number, initial_state_number + 2]
    priors[NodeLabel.THETA_S_SX.format(initial_state_number + 2)] = Dirichlet(
        get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

    # DRW to TGVDR, TYVDR, LRW | LRW, DRW adjacent rooms | HW adjacent Hallways
    valid_transitions = [initial_state_number, range(initial_state_number + 3,
                                                     initial_state_number + 6)] + adjacent_rooms_rw_state_numbers + adjacent_hallways_state_numbers
    priors[NodeLabel.THETA_S_SX.format(initial_state_number + 3)] = Dirichlet(
        get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

    # Keep triaging + back to room walk (same room)
    valid_transitions = [initial_state_number + 3, initial_state_number + 4]
    priors[NodeLabel.THETA_S_SX.format(initial_state_number + 4)] = Dirichlet(
        get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

    # Keep triaging + back to room walk (same room)
    valid_transitions = [initial_state_number + 3, initial_state_number + 5]
    priors[NodeLabel.THETA_S_SX.format(initial_state_number + 5)] = Dirichlet(
        get_one_hot_vector(NUMBER_OF_STATES, valid_transitions))

    return priors


def get_theta_s_priors():
    priors = {}

    # P(theta_s | s_t-1 = HW_some_area)
    priors.update(get_theta_s_prior_per_hallway(0, [], [1]))
    priors.update(get_theta_s_prior_per_hallway(1, [range(8, 32, 3)], [0, 2]))
    priors.update(get_theta_s_prior_per_hallway(2, [], [1, 3, 4, 5]))
    priors.update(get_theta_s_prior_per_hallway(3, [range(56, 98, 3)], [2]))
    priors.update(get_theta_s_prior_per_hallway(4, [range(14, 20, 3), range(32, 38, 3), range(56, 62, 3)], [2, 6]))
    priors.update(get_theta_s_prior_per_hallway(5, [range(74, 80, 3), range(98, 104, 3)], [2, 7]))
    priors.update(get_theta_s_prior_per_hallway(6, [range(14, 20, 3), range(32, 56, 3)], [4]))
    priors.update(get_theta_s_prior_per_hallway(7, [range(98, 116, 3)], [5]))

    # P(theta_s | s_t-1 = state_some_room)
    priors.update(get_theta_s_prior_per_room(8, [range(14, 20, 3)], [1]))
    priors.update(get_theta_s_prior_per_room(14, [range(8, 14, 3)], [1, 4, 6]))
    priors.update(get_theta_s_prior_per_room(20, [range(26, 32, 3)], [1]))
    priors.update(get_theta_s_prior_per_room(26, [range(20, 26, 3)], [1]))
    priors.update(get_theta_s_prior_per_room(32, [range(38, 44, 3), range(56, 68, 3)], [4, 6]))
    priors.update(get_theta_s_prior_per_room(38, [range(32, 38, 3), range(44, 50, 3), range(62, 68, 3)], [6]))
    priors.update(get_theta_s_prior_per_room(44, [range(38, 44, 3), range(50, 56, 3), range(62, 68, 3)], [6]))
    priors.update(get_theta_s_prior_per_room(50, [range(44, 50, 3), range(68, 74, 3)], [6]))
    priors.update(get_theta_s_prior_per_room(56, [range(32, 38, 3), range(62, 68, 3)], [3, 4]))
    priors.update(get_theta_s_prior_per_room(62, [range(32, 50, 3), range(56, 62, 3), range(68, 74, 3)], [3]))
    priors.update(get_theta_s_prior_per_room(68, [range(50, 56, 3), range(62, 68, 3)], [3]))
    priors.update(get_theta_s_prior_per_room(74, [range(80, 86, 3), range(98, 104, 3)], [3, 5]))
    priors.update(get_theta_s_prior_per_room(80, [range(74, 80, 3), range(86, 92, 3), range(98, 104, 3)], [3]))
    priors.update(get_theta_s_prior_per_room(86, [range(80, 86, 3), range(92, 98, 3), range(104, 110, 3)], [3]))
    priors.update(get_theta_s_prior_per_room(92, [range(86, 92, 3), range(110, 116, 3)], [3]))
    priors.update(get_theta_s_prior_per_room(98, [range(74, 86, 3), range(104, 110, 3)], [5, 7]))
    priors.update(get_theta_s_prior_per_room(104, [range(86, 92, 3), range(98, 104, 3), range(110, 116, 3)], [7]))
    priors.update(get_theta_s_prior_per_room(110, [range(92, 98, 3), range(104, 110, 3)], [7]))

    return priors


def get_pi_lt_priors():
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


def get_rooms_distribution_per_state():
    distributions = []

    for s in range(8):
        distributions.append(Multinomial(get_one_hot_vector(NUMBER_OF_ROOMS, [s], zero=0)))

    for s in range(8, 116):
        room_index = int((s - 8) / 6) + 8
        distributions.append(Multinomial(get_one_hot_vector(NUMBER_OF_ROOMS, [room_index], zero=0)))

    return distributions


def create_parameterized_cpds_for_individual_nodes(nodes_metadata, parameter_nodes_metadata):
    cpds = []
    state_nm = nodes_metadata[NodeLabel.S]
    state_parameter_parents = []
    light_parameter_parents = []
    triaged_green_parameter_parents = []
    triaged_yellow_parameter_parents = []
    state_parameterized_distributions = []
    light_parameterized_distributions = []
    triaged_green_parameterized_distributions = []
    triaged_yellow_parameterized_distributions = []

    theta_s_priors = get_theta_s_priors()
    pi_lt_priors = get_pi_lt_priors()

    for s in range(NUMBER_OF_STATES):
        parameter_parent_nm = parameter_nodes_metadata[NodeLabel.THETA_S_SX.format(s)]
        state_parameter_parents.append(parameter_parent_nm)
        state_parameterized_distributions.append(Multinomial(parameter_parent_nm))
        cpds.append(CPD(parameter_parent_nm, [], theta_s_priors[NodeLabel.THETA_S_SX.format(s)]))

        parameter_parent_nm = parameter_nodes_metadata[NodeLabel.PI_LT_SX.format(s)]
        light_parameter_parents.append(parameter_parent_nm)
        light_parameterized_distributions.append(Binomial(parameter_parent_nm))
        cpds.append(CPD(parameter_parent_nm, [], pi_lt_priors[NodeLabel.PI_LT_SX.format(s)]))

        parameter_parent_nm = parameter_nodes_metadata[NodeLabel.PI_TG_SX.format(s)]
        triaged_green_parameter_parents.append(parameter_parent_nm)
        triaged_green_parameterized_distributions.append(Binomial(parameter_parent_nm))
        cpds.append(CPD(parameter_parent_nm, [], Beta(1, 1)))  # Fixed prior

        parameter_parent_nm = parameter_nodes_metadata[NodeLabel.PI_TY_SX.format(s)]
        triaged_yellow_parameter_parents.append(parameter_parent_nm)
        triaged_yellow_parameterized_distributions.append(Binomial(parameter_parent_nm))
        cpds.append(CPD(parameter_parent_nm, [], Beta(1, 1)))  # Fixed prior

    cpds.append(CPD(state_nm, [], Multinomial(
        get_one_hot_vector(NUMBER_OF_STATES, [1], zero=0))))  # The first state is always HW_WaitingRoom
    cpds.append(CPD(state_nm, [state_nm] + state_parameter_parents, state_parameterized_distributions))
    cpds.append(
        CPD(nodes_metadata[NodeLabel.LT], [state_nm] + light_parameter_parents, light_parameterized_distributions))
    cpds.append(CPD(nodes_metadata[NodeLabel.TG], [state_nm] + triaged_green_parameter_parents,
                    triaged_green_parameterized_distributions))
    cpds.append(CPD(nodes_metadata[NodeLabel.TY], [state_nm] + triaged_yellow_parameter_parents,
                    triaged_yellow_parameterized_distributions))
    cpds.append(CPD(nodes_metadata[NodeLabel.RM], [state_nm], get_rooms_distribution_per_state()))

    return cpds


def create_parameterized_cpds_for_multinodes(nodes_metadata, parameter_nodes_metadata):
    cpds = []
    state_nm = nodes_metadata[NodeLabel.S]

    # Parameter nodes for individual victims given a state
    for v in range(NUMBER_OF_GREEN_VICTIMS):
        distance_green_parameter_parents = []
        distance_green_parameterized_distributions = []
        for s in range(NUMBER_OF_STATES):
            parameter_parent_nm = parameter_nodes_metadata[NodeLabel.SIGMA_DGv_SX.format(v, s)]
            distance_green_parameter_parents.append(parameter_parent_nm)
            distance_green_parameterized_distributions.append(Gaussian(0, parameter_parent_nm))
            cpds.append(CPD(parameter_parent_nm, [], InverseGamma(1, 1)))  # Fixed prior

        cpds.append(CPD(nodes_metadata[NodeLabel.DGv.format(v)], [state_nm] + distance_green_parameter_parents,
                        distance_green_parameterized_distributions))

    for v in range(NUMBER_OF_YELLOW_VICTIMS):
        distance_yellow_parameter_parents = []
        distance_yellow_parameterized_distributions = []
        for s in range(NUMBER_OF_STATES):
            parameter_parent_nm = parameter_nodes_metadata[NodeLabel.SIGMA_DYv_SX.format(v, s)]
            distance_yellow_parameter_parents.append(parameter_parent_nm)
            distance_yellow_parameterized_distributions.append(Gaussian(0, parameter_parent_nm))
            cpds.append(CPD(parameter_parent_nm, [], InverseGamma(1, 1)))  # Fixed prior

        cpds.append(CPD(nodes_metadata[NodeLabel.DYv.format(v)], [state_nm] + distance_yellow_parameter_parents,
                        distance_yellow_parameterized_distributions))

    return cpds


def create_parameterized_cpds(nodes_metadata, parameter_nodes_metadata):
    cpds = create_parameterized_cpds_for_individual_nodes(nodes_metadata, parameter_nodes_metadata)
    cpds += create_parameterized_cpds_for_multinodes(nodes_metadata, parameter_nodes_metadata)

    return cpds


def build_pgm(time_slices):
    metadata = PGMMetadata()
    nodes_metadata = create_nodes_metadata()
    parameter_nodes_metadata = create_parameter_nodes_metadata()

    metadata.add_nodes_from(nodes_metadata.values())
    metadata.add_nodes_from(parameter_nodes_metadata.values())
    metadata.add_edges_from(create_edges_from(nodes_metadata))
    metadata.add_edges_from(create_edges_from_parameter_nodes(parameter_nodes_metadata, nodes_metadata))
    metadata.add_cpds_from(create_parameterized_cpds(nodes_metadata, parameter_nodes_metadata))

    pgm = PGM(metadata, time_slices)
    return pgm

class ModelDataBuilder:

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

        initial_time = observations[0]['data']['total_time']
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
            evidence[(NodeLabel.TG, t)] = triaging_green
            evidence[(NodeLabel.TY, t)] = triaging_yellow
            for coordinates, index in data_adapter.GREEN_VICTIMS.items():
                victim_x = coordinates[0]
                victim_y = coordinates[1]
                evidence[(NodeLabel.DGv.format(index), t)] = self.get_distance(victim_x, victim_y, player_x, player_y)
            for coordinates, index in data_adapter.YELLOW_VICTIMS.items():
                victim_x = coordinates[0]
                victim_y = coordinates[1]
                evidence[(NodeLabel.DYv.format(index), t)] = self.get_distance(victim_x, victim_y, player_x, player_y)

        return pd.Series(evidence)

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
        return np.sqrt((victim_x - player_x)**2 + (victim_y - player_y)**2)

if __name__ == '__main__':
    # pgm = build_pgm(600)
    #
    # print('{} Nodes:'.format(len(pgm.nodes)))
    # print(pgm.nodes)
    #
    # print('{} Edges:'.format(len(pgm.edges)))
    # print(pgm.edges)

    model_data_builder = ModelDataBuilder('data/testbed')
    evidence_set = model_data_builder.collect_evidence_from_experiments()
    evidence_set.to_csv('data/evidence/sar_internal_data_collection.csv')
