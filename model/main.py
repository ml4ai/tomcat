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

from base.node import Node

# def build_simple_pgm():
#     d = NodeMetadata('d', cardinality=2, state_names={0: 'easy', 1: 'hard'})
#     i = NodeMetadata('i', cardinality=2, state_names={0: 'dumb', 1: 'smart'})
#     g = NodeMetadata('g', cardinality=3, state_names={0: 'A', 1: 'B', 2: 'C'})
#     s = NodeMetadata('s', cardinality=2, state_names={0: 'Low', 1: 'High'})
#     l = NodeMetadata('l', cardinality=2, state_names={0: 'Bad', 1: 'Good'})
#     nodes = [d, i, g, s, l]
#     e1 = EdgeMetadata(d, g)
#     e2 = EdgeMetadata(i, g)
#     e3 = EdgeMetadata(g, l)
#     e4 = EdgeMetadata(i, s)
#     edges = [e1, e2, e3, e4]
#
#     d_prior = CPD(d, [], [Binomial(0.4)])
#     i_prior = CPD(i, [], [Binomial(0.3)])
#     g_given_d_i = CPD(g, [i, d], [Multinomial([0.3, 0.4]), Multinomial([0.05, 0.25]),
#                                   Multinomial([0.9, 0.08]), Multinomial([0.5, 0.3])])
#     l_given_g = CPD(l, [g], [Binomial(0.9), Binomial(0.6), Binomial(0.01)])
#     s_given_i = CPD(s, [i], [Binomial(0.05), Binomial(0.8)])
#     cpds = [d_prior, i_prior, g_given_d_i, l_given_g, s_given_i]
#
#     metadata = PGMMetadata()
#     metadata.add_nodes_from(nodes)
#     metadata.add_edges_from(edges)
#     metadata.add_cpds_from(cpds)
#
#     pgm = PGM(metadata, 1)
#     return pgm
#
# def build_complete_pgm():
#     d = NodeMetadata('d', cardinality=2, state_names={0: 'easy', 1: 'hard'})
#     i = NodeMetadata('i', cardinality=2, state_names={0: 'dumb', 1: 'smart'})
#     g = NodeMetadata('g', cardinality=3, state_names={0: 'A', 1: 'B', 2: 'C'})
#     s = NodeMetadata('s', cardinality=2, state_names={0: 'Low', 1: 'High'})
#     l = NodeMetadata('l', cardinality=2, state_names={0: 'Bad', 1: 'Good'})
#
#     pi_d = NodeMetadata('pi_d')
#     pi_i = NodeMetadata('pi_i')
#     pi_s0 = NodeMetadata('pi_s0')
#     pi_s1 = NodeMetadata('pi_s1')
#     pi_l0 = NodeMetadata('pi_l0')
#     pi_l1 = NodeMetadata('pi_l1')
#     pi_l2 = NodeMetadata('pi_l2')
#     theta_g0 = NodeMetadata('theta_g0', dimensionality=3)
#     theta_g1 = NodeMetadata('theta_g1', dimensionality=3)
#     theta_g2 = NodeMetadata('theta_g2', dimensionality=3)
#     theta_g3 = NodeMetadata('theta_g3', dimensionality=3)
#
#     nodes = [d, i, g, s, l, pi_d, pi_i, pi_s0, pi_s1, pi_l0, pi_l1, pi_l2, theta_g0, theta_g1, theta_g2, theta_g3]
#
#     e1 = EdgeMetadata(d, g)
#     e2 = EdgeMetadata(i, g)
#     e3 = EdgeMetadata(g, l)
#     e4 = EdgeMetadata(i, s)
#
#     e5 = EdgeMetadata(pi_d, d)
#     e6 = EdgeMetadata(pi_i, i)
#     e7 = EdgeMetadata(pi_s0, s)
#     e8 = EdgeMetadata(pi_s1, s)
#     e9 = EdgeMetadata(pi_l0, l)
#     e10 = EdgeMetadata(pi_l1, l)
#     e11 = EdgeMetadata(pi_l2, l)
#     e12 = EdgeMetadata(theta_g0, g)
#     e13 = EdgeMetadata(theta_g1, g)
#     e14 = EdgeMetadata(theta_g2, g)
#     e15 = EdgeMetadata(theta_g3, g)
#
#     edges = [e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15]
#
#     pi_d_prior = CPD(pi_d, [], [Beta(1, 1)])
#     pi_i_prior = CPD(pi_i, [], [Beta(1, 1)])
#     pi_s0_prior = CPD(pi_s0, [], [Beta(1, 1)])
#     pi_s1_prior = CPD(pi_s1, [], [Beta(1, 1)])
#     pi_l0_prior = CPD(pi_l0, [], [Beta(1, 1)])
#     pi_l1_prior = CPD(pi_l1, [], [Beta(1, 1)])
#     pi_l2_prior = CPD(pi_l2, [], [Beta(1, 1)])
#     theta_g0_prior = CPD(theta_g0, [], [Dirichlet([1, 1, 1])])
#     theta_g1_prior = CPD(theta_g1, [], [Dirichlet([1, 1, 1])])
#     theta_g2_prior = CPD(theta_g2, [], [Dirichlet([1, 1, 1])])
#     theta_g3_prior = CPD(theta_g3, [], [Dirichlet([1, 1, 1])])
#
#     d_prior = CPD(d, [pi_d], [Binomial(Node(pi_d))])
#     i_prior = CPD(i, [pi_i], [Binomial(Node(pi_i))])
#     g_given_d_i = CPD(g, [i, d, theta_g0, theta_g1, theta_g2, theta_g3],
#                       [Multinomial(Node(theta_g0)), Multinomial(Node(theta_g1)),
#                        Multinomial(Node(theta_g2)), Multinomial(Node(theta_g3))])
#     l_given_g = CPD(l, [g, pi_l0, pi_l1, pi_l2], [Binomial(Node(pi_l0)), Binomial(Node(pi_l1)), Binomial(Node(pi_l2))])
#     s_given_i = CPD(s, [i, pi_s0, pi_s1], [Binomial(Node(pi_s0)), Binomial(Node(pi_s1))])
#     cpds = [d_prior, i_prior, g_given_d_i, l_given_g, s_given_i, pi_d_prior, pi_i_prior, pi_s0_prior, pi_s1_prior,
#             pi_l0_prior, pi_l1_prior, pi_l2_prior, theta_g0_prior, theta_g1_prior, theta_g2_prior, theta_g3_prior]
#
#     metadata = PGMMetadata()
#     metadata.add_nodes_from(nodes)
#     metadata.add_edges_from(edges)
#     metadata.add_cpds_from(cpds)
#
#     pgm = PGM(metadata, 1)
#     return pgm
#
# def estimate_g_from_gibbs_sampling(pgm):
#     sampling = GibbsSampling(pgm)
#     observations = pd.DataFrame([{('s', 0):0, ('l', 0):1},{('s', 0):0, ('l', 0):1},{('s', 0):0, ('l', 0):1},{('s', 0):0, ('l', 0):1},{('s', 0):0, ('l', 0):1}])
#     samples, _ = sampling.sample(500, 50, observations)
#     samples.to_csv('toy_grade_500_samples.csv')

def generate_synthetic_data(pgm_metadata, time_slices, number_of_samples):
    pgm = PGM(pgm_metadata, time_slices)

    sampling = AncestralSampling(pgm)
    samples, _ = sampling.sample(number_of_samples)
    return samples

if __name__ == '__main__':
    random.seed(42)
    np.random.seed(42)
    # pgm = build_simple_pgm()
    # estimate_g_from_gibbs_sampling(pgm)
    # pgm = build_complete_pgm()
    # nx.draw_shell(pgm, with_labels=True)
    # plt.show()
    # estimate_g_from_gibbs_sampling(pgm)
    pgm_metadata = student.build_pgm()
    samples = generate_synthetic_data(pgm_metadata, 1, 500)
    samples.to_csv('student_500.csv'.format())
    samples.drop(('g',0), axis=1, inplace=True)

    parameter_estimation = student.estimate_parameters_from_samples(samples, 50, 500)
    parameter_estimation.to_csv('student_param_estimation_no_g_500_500.csv'.format())
