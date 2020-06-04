from distribution.discrete.multinomial import Multinomial
from distribution.discrete.binomial import Binomial
from distribution.continuous.beta import Beta
from distribution.continuous.dirichlet import Dirichlet
from base.node_metadata import NodeMetadata
from base.edge_metadata import EdgeMetadata
from distribution.cpd import CPD
from model.pgm_metadata import PGMMetadata
from base.node import Node
from model.pgm import PGM
from estimator.gibbs_estimator import GibbsEstimator
import pandas as pd

class NodeLabel:
    D = 'd'
    I = 'i'
    G = 'g'
    S = 's'
    L = 'l'
    PRIOR_D = 'prior_d'
    PRIOR_I = 'prior_i'
    PRIOR_S_I0 = 'prior_s_i0'
    PRIOR_S_I1 = 'prior_s_i1'
    PRIOR_L_G0 = 'prior_l_g0'
    PRIOR_L_G1 = 'prior_l_g1'
    PRIOR_L_G2 = 'prior_l_g2'
    PRIOR_G_D0I0 = 'prior_g_d0i0'
    PRIOR_G_D0I1 = 'prior_g_d0i1'
    PRIOR_G_D1I0 = 'prior_g_d1i0'
    PRIOR_G_D1I1 = 'prior_g_d1i1'


def build_pgm(complete_generative_model=False):
    d = NodeMetadata(NodeLabel.D, cardinality=2, state_names={0: 'easy', 1: 'hard'})
    i = NodeMetadata(NodeLabel.I, cardinality=2, state_names={0: 'dumb', 1: 'smart'})
    g = NodeMetadata(NodeLabel.G, cardinality=3, state_names={0: 'A', 1: 'B', 2: 'C'})
    s = NodeMetadata(NodeLabel.S, cardinality=2, state_names={0: 'Low', 1: 'High'})
    l = NodeMetadata(NodeLabel.L, cardinality=2, state_names={0: 'Bad', 1: 'Good'})
    nodes = [d, i, g, s, l]

    edges = []
    edges.append(EdgeMetadata(d, g))
    edges.append(EdgeMetadata(i, g))
    edges.append(EdgeMetadata(g, l))
    edges.append(EdgeMetadata(i, s))

    cpds = []
    if complete_generative_model:
        # Priors
        prior_d = NodeMetadata(NodeLabel.PRIOR_D, parameter=True, constant=True)
        prior_i = NodeMetadata(NodeLabel.PRIOR_I, parameter=True, constant=True)
        prior_s_i0 = NodeMetadata(NodeLabel.PRIOR_S_I0, parameter=True, constant=True)
        prior_s_i1 = NodeMetadata(NodeLabel.PRIOR_S_I1, parameter=True, constant=True)
        prior_l_g0 = NodeMetadata(NodeLabel.PRIOR_L_G0, parameter=True, constant=True)
        prior_l_g1 = NodeMetadata(NodeLabel.PRIOR_L_G1, parameter=True, constant=True)
        prior_l_g2 = NodeMetadata(NodeLabel.PRIOR_L_G2, parameter=True, constant=True)
        prior_g_d0i0 = NodeMetadata(NodeLabel.PRIOR_G_D0I0, parameter=True, constant=True)
        prior_g_d0i1 = NodeMetadata(NodeLabel.PRIOR_G_D0I1, parameter=True, constant=True)
        prior_g_d1i0 = NodeMetadata(NodeLabel.PRIOR_G_D1I0, parameter=True, constant=True)
        prior_g_d1i1 = NodeMetadata(NodeLabel.PRIOR_G_D1I1, parameter=True, constant=True)
        nodes += [prior_d, prior_i, prior_s_i0, prior_s_i1, prior_l_g0, prior_l_g1, prior_l_g2, prior_g_d0i0,
                  prior_g_d0i1, prior_g_d1i0, prior_g_d1i1]

        # Connections from priors to variables
        edges.append(EdgeMetadata(prior_d, d))
        edges.append(EdgeMetadata(prior_i, i))
        edges.append(EdgeMetadata(prior_s_i0, s))
        edges.append(EdgeMetadata(prior_s_i1, s))
        edges.append(EdgeMetadata(prior_l_g0, l))
        edges.append(EdgeMetadata(prior_l_g1, l))
        edges.append(EdgeMetadata(prior_l_g2, l))
        edges.append(EdgeMetadata(prior_g_d0i0, g))
        edges.append(EdgeMetadata(prior_g_d0i1, g))
        edges.append(EdgeMetadata(prior_g_d1i0, g))
        edges.append(EdgeMetadata(prior_g_d1i1, g))

        # Priors CPDs
        cpds.append(CPD(prior_d, [], [Beta(1, 1)]))
        cpds.append(CPD(prior_i, [], [Beta(1, 1)]))
        cpds.append(CPD(prior_s_i0, [], [Beta(1, 1)]))
        cpds.append(CPD(prior_s_i1, [], [Beta(1, 1)]))
        cpds.append(CPD(prior_l_g0, [], [Beta(1, 1)]))
        cpds.append(CPD(prior_l_g1, [], [Beta(1, 1)]))
        cpds.append(CPD(prior_l_g2, [], [Beta(1, 1)]))
        cpds.append(CPD(prior_g_d0i0, [], [Dirichlet([1, 1, 1])]))
        cpds.append(CPD(prior_g_d0i1, [], [Dirichlet([1, 1, 1])]))
        cpds.append(CPD(prior_g_d1i0, [], [Dirichlet([1, 1, 1])]))
        cpds.append(CPD(prior_g_d1i1, [], [Dirichlet([1, 1, 1])]))

        cpds.append(CPD(d, [prior_d], [Binomial(Node(prior_d))]))
        cpds.append(CPD(i, [prior_i], [Binomial(Node(prior_i))]))
        cpds.append(CPD(g, [d, i, prior_g_d0i0, prior_g_d0i1, prior_g_d1i0, prior_g_d1i1],
                        [Multinomial(Node(prior_g_d0i0)), Multinomial(Node(prior_g_d0i1)),
                         Multinomial(Node(prior_g_d1i0)), Multinomial(Node(prior_g_d1i1))]))
        cpds.append(
            CPD(l, [g, prior_l_g0, prior_l_g1, prior_l_g2],
                [Binomial(Node(prior_l_g0)), Binomial(Node(prior_l_g1)), Binomial(Node(prior_l_g2))]))
        cpds.append(CPD(s, [i, prior_s_i0, prior_s_i1], [Binomial(Node(prior_s_i0)), Binomial(Node(prior_s_i1))]))
    else:
        cpds.append(CPD(d, [], [Binomial(0.4)]))
        cpds.append(CPD(i, [], [Binomial(0.3)]))
        cpds.append(CPD(g, [d, i],
                        [Multinomial([0.3, 0.4, 0.3]), Multinomial([0.9, 0.08, 0.02]), Multinomial([0.05, 0.25, 0.7]),
                         Multinomial([0.5, 0.3, 0.2])]))
        cpds.append(CPD(l, [g], [Binomial(0.9), Binomial(0.6), Binomial(0.01)]))
        cpds.append(CPD(s, [i], [Binomial(0.05), Binomial(0.8)]))

    metadata = PGMMetadata()
    metadata.add_nodes_from(nodes)
    metadata.add_edges_from(edges)
    metadata.add_cpds_from(cpds)

    return metadata

def estimate_parameters_from_samples(samples, burn_in_periods, number_of_samples):
    pgm = PGM(build_pgm(True), 1)
    estimator = GibbsEstimator(pgm)
    parameter_samples = estimator.estimate_parameters(samples, number_of_samples, burn_in_periods)
    parameter_samples.to_csv('tmp.csv')

    parameter_estimation = parameter_samples.mean()
    parameter_estimation.index = pd.MultiIndex.from_tuples(parameter_estimation.index)
    parameter_estimation = parameter_estimation.droplevel(1)  # Remove time slice indication

    return parameter_estimation
