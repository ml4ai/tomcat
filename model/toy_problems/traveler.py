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
    CITY = 'city'
    OBS_CITY = 'observed_city'
    PRIOR_CITY = 'prior_city'
    PARAM_CITY_A_TO_CITY = 'param_city_a_to_city'
    PARAM_CITY_B_TO_CITY = 'param_city_b_to_city'
    PARAM_CITY_C_TO_CITY = 'param_city_c_to_city'
    PARAM_CITY_D_TO_CITY = 'param_city_d_to_city'
    PARAM_CITY_A_TO_SENSOR_A = 'param_city_a_to_sensor_a'
    PARAM_CITY_B_TO_SENSOR_A = 'param_city_b_to_sensor_a'
    PARAM_CITY_C_TO_SENSOR_A = 'param_city_c_to_sensor_a'
    PARAM_CITY_D_TO_SENSOR_A = 'param_city_d_to_sensor_a'
    PARAM_CITY_A_TO_SENSOR_B = 'param_city_a_to_sensor_b'
    PARAM_CITY_B_TO_SENSOR_B = 'param_city_b_to_sensor_b'
    PARAM_CITY_C_TO_SENSOR_B = 'param_city_c_to_sensor_b'
    PARAM_CITY_D_TO_SENSOR_B = 'param_city_d_to_sensor_b'
    PARAM_CITY_A_TO_SENSOR_C = 'param_city_a_to_sensor_c'
    PARAM_CITY_B_TO_SENSOR_C = 'param_city_b_to_sensor_c'
    PARAM_CITY_C_TO_SENSOR_C = 'param_city_c_to_sensor_c'
    PARAM_CITY_D_TO_SENSOR_C = 'param_city_d_to_sensor_c'
    PARAM_CITY_A_TO_SENSOR_D = 'param_city_a_to_sensor_d'
    PARAM_CITY_B_TO_SENSOR_D = 'param_city_b_to_sensor_d'
    PARAM_CITY_C_TO_SENSOR_D = 'param_city_c_to_sensor_d'
    PARAM_CITY_D_TO_SENSOR_D = 'param_city_d_to_sensor_d'
    SENSOR_A = 'sensor_a'
    SENSOR_B = 'sensor_b'
    SENSOR_C = 'sensor_c'
    SENSOR_D = 'sensor_d'

def build_pgm(complete_generative_model=False):
    city = NodeMetadata(NodeLabel.CITY, cardinality=4, repeatable=True)
    sensor_a = NodeMetadata(NodeLabel.SENSOR_A, cardinality=2, repeatable=True)
    sensor_b = NodeMetadata(NodeLabel.SENSOR_B, cardinality=2, repeatable=True)
    sensor_c = NodeMetadata(NodeLabel.SENSOR_C, cardinality=2, repeatable=True)
    sensor_d = NodeMetadata(NodeLabel.SENSOR_D, cardinality=2, repeatable=True)
    nodes = [city, sensor_a, sensor_b, sensor_c, sensor_d]

    edges = []
    edges.append(EdgeMetadata(city, city, forward_in_time=True))
    edges.append(EdgeMetadata(city, sensor_a))
    edges.append(EdgeMetadata(city, sensor_b))
    edges.append(EdgeMetadata(city, sensor_c))
    edges.append(EdgeMetadata(city, sensor_d))

    cpds = []
    if complete_generative_model:
        # Priors
        prior_city = NodeMetadata(NodeLabel.PRIOR_CITY, parameter=True, constant=True, prior=True)
        param_city_a_to_city = NodeMetadata(NodeLabel.PARAM_CITY_A_TO_CITY, parameter=True, constant=True,
                                            first_time_slice=1)
        param_city_b_to_city = NodeMetadata(NodeLabel.PARAM_CITY_B_TO_CITY, parameter=True, constant=True,
                                            first_time_slice=1)
        param_city_c_to_city = NodeMetadata(NodeLabel.PARAM_CITY_C_TO_CITY, parameter=True, constant=True,
                                            first_time_slice=1)
        param_city_d_to_city = NodeMetadata(NodeLabel.PARAM_CITY_D_TO_CITY, parameter=True, constant=True,
                                            first_time_slice=1)
        param_city_a_to_sensor_a = NodeMetadata(NodeLabel.PARAM_CITY_A_TO_SENSOR_A, parameter=True, constant=True)
        param_city_b_to_sensor_a = NodeMetadata(NodeLabel.PARAM_CITY_B_TO_SENSOR_A, parameter=True, constant=True)
        param_city_c_to_sensor_a = NodeMetadata(NodeLabel.PARAM_CITY_C_TO_SENSOR_A, parameter=True, constant=True)
        param_city_d_to_sensor_a = NodeMetadata(NodeLabel.PARAM_CITY_D_TO_SENSOR_A, parameter=True, constant=True)
        param_city_a_to_sensor_b = NodeMetadata(NodeLabel.PARAM_CITY_A_TO_SENSOR_B, parameter=True, constant=True)
        param_city_b_to_sensor_b = NodeMetadata(NodeLabel.PARAM_CITY_B_TO_SENSOR_B, parameter=True, constant=True)
        param_city_c_to_sensor_b = NodeMetadata(NodeLabel.PARAM_CITY_C_TO_SENSOR_B, parameter=True, constant=True)
        param_city_d_to_sensor_b = NodeMetadata(NodeLabel.PARAM_CITY_D_TO_SENSOR_B, parameter=True, constant=True)
        param_city_a_to_sensor_c = NodeMetadata(NodeLabel.PARAM_CITY_A_TO_SENSOR_C, parameter=True, constant=True)
        param_city_b_to_sensor_c = NodeMetadata(NodeLabel.PARAM_CITY_B_TO_SENSOR_C, parameter=True, constant=True)
        param_city_c_to_sensor_c = NodeMetadata(NodeLabel.PARAM_CITY_C_TO_SENSOR_C, parameter=True, constant=True)
        param_city_d_to_sensor_c = NodeMetadata(NodeLabel.PARAM_CITY_D_TO_SENSOR_C, parameter=True, constant=True)
        param_city_a_to_sensor_d = NodeMetadata(NodeLabel.PARAM_CITY_A_TO_SENSOR_D, parameter=True, constant=True)
        param_city_b_to_sensor_d = NodeMetadata(NodeLabel.PARAM_CITY_B_TO_SENSOR_D, parameter=True, constant=True)
        param_city_c_to_sensor_d = NodeMetadata(NodeLabel.PARAM_CITY_C_TO_SENSOR_D, parameter=True, constant=True)
        param_city_d_to_sensor_d = NodeMetadata(NodeLabel.PARAM_CITY_D_TO_SENSOR_D, parameter=True, constant=True)

        nodes += [prior_city, param_city_a_to_city, param_city_b_to_city, param_city_c_to_city, param_city_d_to_city,
                  param_city_a_to_sensor_a, param_city_b_to_sensor_a, param_city_c_to_sensor_a,
                  param_city_d_to_sensor_a,
                  param_city_a_to_sensor_b, param_city_b_to_sensor_b, param_city_c_to_sensor_b,
                  param_city_d_to_sensor_b,
                  param_city_a_to_sensor_c, param_city_b_to_sensor_c, param_city_c_to_sensor_c,
                  param_city_d_to_sensor_c,
                  param_city_a_to_sensor_d, param_city_b_to_sensor_d, param_city_c_to_sensor_d,
                  param_city_d_to_sensor_d]

        # Connections from parameters to variables
        edges.append(EdgeMetadata(prior_city, city))
        edges.append(EdgeMetadata(param_city_a_to_city, city))
        edges.append(EdgeMetadata(param_city_b_to_city, city))
        edges.append(EdgeMetadata(param_city_c_to_city, city))
        edges.append(EdgeMetadata(param_city_d_to_city, city))
        edges.append(EdgeMetadata(param_city_a_to_sensor_a, sensor_a))
        edges.append(EdgeMetadata(param_city_b_to_sensor_a, sensor_a))
        edges.append(EdgeMetadata(param_city_c_to_sensor_a, sensor_a))
        edges.append(EdgeMetadata(param_city_d_to_sensor_a, sensor_a))
        edges.append(EdgeMetadata(param_city_a_to_sensor_b, sensor_b))
        edges.append(EdgeMetadata(param_city_b_to_sensor_b, sensor_b))
        edges.append(EdgeMetadata(param_city_c_to_sensor_b, sensor_b))
        edges.append(EdgeMetadata(param_city_d_to_sensor_b, sensor_b))
        edges.append(EdgeMetadata(param_city_a_to_sensor_c, sensor_c))
        edges.append(EdgeMetadata(param_city_b_to_sensor_c, sensor_c))
        edges.append(EdgeMetadata(param_city_c_to_sensor_c, sensor_c))
        edges.append(EdgeMetadata(param_city_d_to_sensor_c, sensor_c))
        edges.append(EdgeMetadata(param_city_a_to_sensor_d, sensor_d))
        edges.append(EdgeMetadata(param_city_b_to_sensor_d, sensor_d))
        edges.append(EdgeMetadata(param_city_c_to_sensor_d, sensor_d))
        edges.append(EdgeMetadata(param_city_d_to_sensor_d, sensor_d))

        # Parameter CPDs
        cpds.append(CPD(prior_city, [], [Dirichlet([1, 1, 1, 1])]))
        cpds.append(CPD(param_city_a_to_city, [], [Dirichlet([1, 1, 1, 1])]))
        cpds.append(CPD(param_city_b_to_city, [], [Dirichlet([1, 1, 1, 1])]))
        cpds.append(CPD(param_city_c_to_city, [], [Dirichlet([1, 1, 1, 1])]))
        cpds.append(CPD(param_city_d_to_city, [], [Dirichlet([1, 1, 1, 1])]))
        cpds.append(CPD(param_city_a_to_sensor_a, [], [Beta(1, 5)]))
        cpds.append(CPD(param_city_b_to_sensor_a, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_c_to_sensor_a, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_d_to_sensor_a, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_a_to_sensor_b, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_b_to_sensor_b, [], [Beta(1, 5)]))
        cpds.append(CPD(param_city_c_to_sensor_b, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_d_to_sensor_b, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_a_to_sensor_c, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_b_to_sensor_c, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_c_to_sensor_c, [], [Beta(1, 5)]))
        cpds.append(CPD(param_city_d_to_sensor_c, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_a_to_sensor_d, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_b_to_sensor_d, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_c_to_sensor_d, [], [Beta(3, 1)]))
        cpds.append(CPD(param_city_d_to_sensor_d, [], [Beta(1, 5)]))

        cpds.append(CPD(city, [prior_city], [Multinomial(Node(prior_city))]))
        cpds.append(CPD(city,
                        [city, param_city_a_to_city, param_city_b_to_city, param_city_c_to_city, param_city_d_to_city],
                        [Multinomial(Node(param_city_a_to_city)), Multinomial(Node(param_city_b_to_city)),
                         Multinomial(Node(param_city_c_to_city)), Multinomial(Node(param_city_d_to_city))]))
        cpds.append(CPD(sensor_a,
                        [city, param_city_a_to_sensor_a, param_city_b_to_sensor_a, param_city_c_to_sensor_a,
                         param_city_d_to_sensor_a],
                        [Binomial(Node(param_city_a_to_sensor_a)), Binomial(Node(param_city_b_to_sensor_a)),
                         Binomial(Node(param_city_c_to_sensor_a)), Binomial(Node(param_city_d_to_sensor_a))]))
        cpds.append(CPD(sensor_b,
                        [city, param_city_a_to_sensor_b, param_city_b_to_sensor_b, param_city_c_to_sensor_b,
                         param_city_d_to_sensor_b],
                        [Binomial(Node(param_city_a_to_sensor_b)), Binomial(Node(param_city_b_to_sensor_b)),
                         Binomial(Node(param_city_c_to_sensor_b)), Binomial(Node(param_city_d_to_sensor_b))]))
        cpds.append(CPD(sensor_c,
                        [city, param_city_a_to_sensor_c, param_city_b_to_sensor_c, param_city_c_to_sensor_c,
                         param_city_d_to_sensor_c],
                        [Binomial(Node(param_city_a_to_sensor_c)), Binomial(Node(param_city_b_to_sensor_c)),
                         Binomial(Node(param_city_c_to_sensor_c)), Binomial(Node(param_city_d_to_sensor_c))]))
        cpds.append(CPD(sensor_d,
                        [city, param_city_a_to_sensor_d, param_city_b_to_sensor_d, param_city_c_to_sensor_d,
                         param_city_d_to_sensor_d],
                        [Binomial(Node(param_city_a_to_sensor_d)), Binomial(Node(param_city_b_to_sensor_d)),
                         Binomial(Node(param_city_c_to_sensor_d)), Binomial(Node(param_city_d_to_sensor_d))]))

    else:
        # CPDs
        cpds.append(CPD(city, [], [Multinomial([1, 0, 0, 0])]))
        cpds.append(CPD(city, [city],
                        [Multinomial([0, 1, 0, 0]), Multinomial([1 / 3, 0, 1 / 3, 1 / 3]),
                         Multinomial([0, 0.5, 0, 0.5]), Multinomial([0, 0.5, 0.5, 0])]))
        cpds.append(CPD(sensor_a, [city],
                        [Binomial(0.85), Binomial(0.25), Binomial(0.25), Binomial(0.25)]))
        cpds.append(CPD(sensor_b, [city],
                        [Binomial(0.25), Binomial(0.85), Binomial(0.25), Binomial(0.25)]))
        cpds.append(CPD(sensor_c, [city],
                        [Binomial(0.25), Binomial(0.25), Binomial(0.85), Binomial(0.25)]))
        cpds.append(CPD(sensor_d, [city],
                        [Binomial(0.25), Binomial(0.25), Binomial(0.25), Binomial(0.85)]))

    metadata = PGMMetadata()
    metadata.add_nodes_from(nodes)
    metadata.add_edges_from(edges)
    metadata.add_cpds_from(cpds)

    return metadata


def estimate_parameters_from(data, burn_in_periods, number_of_samples):
    pgm = PGM(build_pgm(True), 11)
    estimator = GibbsEstimator(pgm)
    return estimator.estimate_parameters_from(pgm, data, burn_in_periods, number_of_samples)
