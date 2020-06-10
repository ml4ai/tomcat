import numpy as np
from distribution.discrete.multinomial import Multinomial
from distribution.discrete.binomial import Binomial
from distribution.continuous.beta import Beta
from distribution.continuous.dirichlet import Dirichlet
from base.node_metadata import NodeMetadata
from base.edge_metadata import EdgeMetadata
from distribution.cpd import CPD
from model.pgm_metadata import PGMMetadata
from model.pgm import PGM
import random
from sampling.gibbs_sampling import GibbsSampling
from sampling.ancestral_sampling import AncestralSampling
import pandas as pd
import toy_problems.student_network as student
import toy_problems.traveler as traveler
import time
import shlex

from base.node import Node


def generate_synthetic_data(pgm, number_of_samples, out_filename):
    sampling = AncestralSampling(pgm)
    samples = sampling.sample(number_of_samples)
    samples.to_csv(out_filename)


def estimate_student_net_parameters(data, out_filepath, burn_in_periods, number_of_samples):
    estimated_parameters = student.estimate_parameters_from(data, burn_in_periods, number_of_samples)
    estimated_parameters.to_csv(out_filepath)


def estimate_traveler_net_parameters(data, out_filepath, burn_in_periods, number_of_samples):
    estimated_parameters = traveler.estimate_parameters_from(data, burn_in_periods, number_of_samples)
    estimated_parameters.to_csv(out_filepath)


def read_parameters_from_file(filename):
    parameter_values = pd.read_csv(filename, index_col=0).transpose().iloc[0]
    for node_id in parameter_values.index:
        value = [float(v) for v in shlex.split(parameter_values[node_id].strip('[]'))]
        if len(value) == 1:
            value = value[0]
        parameter_values[node_id] = value

    return parameter_values


def read_data_from_file(filepath):
    data = pd.read_csv(filepath, index_col=0)
    data.columns = [eval(column) for column in data.columns]
    return data


def run_student_experiment():
    random.seed(42)
    np.random.seed(42)

    NUM_DATA_POINTS = 1000;
    NUM_SAMPLES = 1500;
    BURN_IN = 150;

    DATA_FILEPATH = 'data/student_net_data_{}dp.csv'.format(NUM_DATA_POINTS)
    DATA_FILEPATH_FROM_PAR_NO_G = 'data/student_net_data_{}dp_{}s_no_g.csv'.format(NUM_DATA_POINTS, NUM_SAMPLES)
    PARAMETERS_FILEPATH_FULL = 'data/student_net_params_{}dp_{}s_full.csv'.format(NUM_DATA_POINTS, NUM_SAMPLES)
    PARAMETERS_FILEPATH_NO_G = 'data/student_net_params_{}dp_{}s_no_g.csv'.format(NUM_DATA_POINTS, NUM_SAMPLES)

    # Generate a bunch of data to work with
    student_network = PGM(student.build_pgm(), 1)

    print("Generating synthetic data")
    # generate_synthetic_data(student_network, NUM_DATA_POINTS, DATA_FILEPATH)

    data = read_data_from_file(DATA_FILEPATH)
    print("Estimating parameters with full observation")
    # estimate_student_net_parameters(data, PARAMETERS_FILEPATH_FULL, BURN_IN, NUM_SAMPLES)

    print("Estimating parameters with G hidden")
    data.drop((student.NodeLabel.G, 0), axis=1, inplace=True)
    estimate_student_net_parameters(data, PARAMETERS_FILEPATH_NO_G, BURN_IN, NUM_SAMPLES)

    parameters = read_parameters_from_file(PARAMETERS_FILEPATH_NO_G)
    student_network.fill_parameters(parameters)

    print("Generating synthetic data for parameters estimated with G hidden")
    generate_synthetic_data(student_network, NUM_DATA_POINTS, DATA_FILEPATH_FROM_PAR_NO_G)


def run_traveler_experiment():
    random.seed(42)
    np.random.seed(42)

    NUM_DATA_POINTS = 1000;
    NUM_SAMPLES = 1000;
    BURN_IN = 100;

    DATA_FILEPATH = 'data/traveler_net_data_{}dp.csv'.format(NUM_DATA_POINTS)
    DATA_FILEPATH_FROM_PAR_NO_CITY = 'data/traveler_net_data_{}dp_{}s_no_city.csv'.format(NUM_DATA_POINTS, NUM_SAMPLES)
    PARAMETERS_FILEPATH_FULL = 'data/traveler_net_params_{}dp_{}s_full.csv'.format(NUM_DATA_POINTS, NUM_SAMPLES)
    PARAMETERS_FILEPATH_NO_CITY = 'data/traveler_net_params_{}dp_{}s_no_city.csv'.format(NUM_DATA_POINTS, NUM_SAMPLES)
    INFERENCE1_FILEPATH = 'data/traveler_net_inference1_{}dp_{}s.csv'.format(NUM_DATA_POINTS, NUM_SAMPLES)

    # Generate a bunch of data to work with
    traveler_network = PGM(traveler.build_pgm(), 5)

    sampling = GibbsSampling(traveler_network)
    # observations = pd.Series(
    #     {('sensor_a', 0): 1, ('sensor_b', 0): 0, ('sensor_c', 0): 0, ('sensor_d', 0): 0, ('sensor_e', 0): 0,
    #      ('sensor_f', 0): 0, ('sensor_g', 0): 0, ('sensor_g', 0): 0, ('sensor_h', 0): 0, ('sensor_a', 1): 0,
    #      ('sensor_b', 1): 1, ('sensor_c', 1): 0, ('sensor_d', 1): 0, ('sensor_e', 1): 0, ('sensor_f', 1): 0,
    #      ('sensor_g', 1): 0, ('sensor_h', 1): 0, ('sensor_a', 2): 0, ('sensor_b', 2): 0, ('sensor_c', 2): 1,
    #      ('sensor_d', 2): 0, ('sensor_e', 2): 0, ('sensor_f', 2): 0, ('sensor_g', 2): 0, ('sensor_g', 2): 0,
    #      ('sensor_h', 2): 0, ('sensor_a', 3): 0, ('sensor_b', 3): 0, ('sensor_c', 3): 0, ('sensor_d', 3): 1,
    #      ('sensor_e', 3): 0, ('sensor_f', 3): 0, ('sensor_g', 3): 0, ('sensor_h', 3): 0, ('sensor_a', 4): 0,
    #      ('sensor_b', 4): 1, ('sensor_c', 4): 0, ('sensor_d', 4): 0, ('sensor_e', 4): 0, ('sensor_f', 4): 0,
    #      ('sensor_g', 4): 0, ('sensor_h', 4): 0, ('sensor_a', 5): 0, ('sensor_b', 5): 0, ('sensor_c', 5): 1,
    #      ('sensor_d', 5): 0, ('sensor_e', 5): 0, ('sensor_f', 5): 0, ('sensor_g', 5): 0, ('sensor_h', 5): 0,
    #      ('sensor_a', 6): 0, ('sensor_b', 6): 0, ('sensor_c', 6): 0, ('sensor_d', 6): 1, ('sensor_e', 6): 0,
    #      ('sensor_f', 6): 0, ('sensor_g', 6): 0, ('sensor_h', 6): 0, ('sensor_a', 7): 0, ('sensor_b', 7): 1,
    #      ('sensor_c', 7): 0, ('sensor_d', 7): 0, ('sensor_e', 7): 0, ('sensor_f', 7): 0, ('sensor_g', 7): 0,
    #      ('sensor_h', 7): 0, ('sensor_a', 8): 0, ('sensor_b', 8): 0, ('sensor_c', 8): 0, ('sensor_d', 8): 0,
    #      ('sensor_e', 8): 1, ('sensor_f', 8): 0, ('sensor_g', 8): 0, ('sensor_h', 8): 0, ('sensor_a', 9): 0,
    #      ('sensor_b', 9): 0, ('sensor_c', 9): 0, ('sensor_d', 9): 0, ('sensor_e', 9): 0, ('sensor_f', 9): 1,
    #      ('sensor_g', 9): 0, ('sensor_h', 9): 0, ('sensor_a', 10): 0, ('sensor_b', 10): 0, ('sensor_c', 10): 0,
    #      ('sensor_d', 10): 0, ('sensor_e', 10): 0, ('sensor_f', 10): 0, ('sensor_g', 10): 0, ('sensor_h', 10): 1})
    # observations = pd.Series(
    #     {('city', 0): 0, ('city', 1): 1, ('city', 2): 2, ('city', 3): 3, ('city', 4): 1, ('city', 5): 2, ('city', 6): 3,
    #      ('city', 7): 1, ('city', 8): 4, ('city', 9): 5, ('city', 10): 7, })
    # samples = sampling.sample(NUM_SAMPLES, BURN_IN, observations=observations)
    # samples.to_csv(INFERENCE1_FILEPATH)

    # print("Generating synthetic data")
    # generate_synthetic_data(traveler_network, NUM_DATA_POINTS, DATA_FILEPATH)
    # #
    # data = read_data_from_file(DATA_FILEPATH)
    # print("Estimating parameters with full observation")
    # estimate_traveler_net_parameters(data, PARAMETERS_FILEPATH_FULL, BURN_IN, NUM_SAMPLES)
    # #
    # print("Estimating parameters with city hidden")
    # data.drop([(traveler.NodeLabel.CITY, t) for t in range(5)], axis=1, inplace=True)
    # estimate_traveler_net_parameters(data, PARAMETERS_FILEPATH_NO_CITY, BURN_IN, NUM_SAMPLES)
    #
    parameters = read_parameters_from_file(PARAMETERS_FILEPATH_NO_CITY)
    traveler_network.fill_parameters(parameters)

    print("Generating synthetic data for parameters estimated with City hidden")
    generate_synthetic_data(traveler_network, NUM_DATA_POINTS, DATA_FILEPATH_FROM_PAR_NO_CITY)


if __name__ == '__main__':
    # run_student_experiment()
    run_traveler_experiment()
