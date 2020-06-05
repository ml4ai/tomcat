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
import time
import shlex

from base.node import Node

def generate_synthetic_data(pgm, number_of_samples):
    sampling = AncestralSampling(pgm)
    samples = sampling.sample(number_of_samples)
    return samples

def read_parameters_from_file(filename):
    parameter_values = pd.read_csv(filename, index_col=0).transpose().iloc[0]
    for node_id in parameter_values.index:
        value = [float(v) for v in shlex.split(parameter_values[node_id].strip('[]'))]
        if len(value) == 1:
            value = value[0]
        parameter_values[node_id] = value

    return parameter_values

if __name__ == '__main__':
    random.seed(42)
    np.random.seed(42)

    number_of_samples = 500;

    # Generate a bunch of data to work with
    pgm_metadata = student.build_pgm(True)
    pgm = PGM(pgm_metadata, 1)

    # print("Generating data...")
    # start = time.process_time()
    # samples = generate_synthetic_data(pgm, number_of_samples)
    # samples.to_csv('student_{}.csv'.format(number_of_samples))
    # end = time.process_time()
    # elapsed_time = end - start
    # print("Processing time: {} seconds".format(elapsed_time))

    # Estimate the parameters using the complete data
    # print("Estimating parameters on complete data...")
    # start = time.process_time()
    # parameter_estimation = student.estimate_parameters_from_samples(samples, int(number_of_samples / 10), number_of_samples)
    # parameter_estimation.to_csv('student_param_estimation_500.csv'.format())
    # end = time.process_time()
    # elapsed_time = end - start
    # print("Processing time: {} seconds".format(elapsed_time))

    # Estimate the parameters using the data excluding g
    # samples.drop([('g',0)], axis=1, inplace=True)
    # start = time.process_time()
    # parameter_estimation = student.estimate_parameters_from_samples(samples, int(number_of_samples / 10), number_of_samples)
    # parameter_estimation.to_csv('student_param_estimation_no_g_500_faster.csv'.format())
    # end = time.process_time()
    # elapsed_time = end - start
    # print("Processing time: {} seconds".format(elapsed_time))

    parameter_values = read_parameters_from_file('data/student_param_estimation_no_g_500_faster.csv')
    pgm.fill_parameters(parameter_values)
    samples = generate_synthetic_data(pgm, number_of_samples)
    samples.to_csv('student_{}_from_estimated_no_g.csv'.format(number_of_samples))

