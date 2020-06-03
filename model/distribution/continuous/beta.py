from distribution.distribution import Distribution
from scipy.stats import beta
from base.node import Node
from distribution.discrete.binomial import Binomial
import copy


class Beta(Distribution):
    """
    Class that represents a Beta distribution
    """

    def __init__(self, a, b):
        self.a = a
        self.b = b

    def sample(self):
        a, b = self.get_concrete_parameters()
        return beta(a, b).rvs()

    def get_concrete_parameters(self):
        """
        Retrieves the parameters.
        """

        if isinstance(self.a, Node):
            a = self.a.assignment
        else:
            a = self.a

        if isinstance(self.b, Node):
            b = self.b.assignment
        else:
            b = self.b

        return a, b

    def get_probability(self, category_frequencies):
        """
        Retrieves the density of a given state
        """
        a, b = self.get_concrete_parameters()
        density = 1
        for category, frequency in category_frequencies.iteritems():
            density *= beta(a, b).pdf(category)**frequency

        return density

    def mult(self, other_distribution):
        raise TypeError('Multiplication not yet implemented for Beta distributions.')

    def power(self, exponent):
        raise TypeError('Power not yet implemented for Beta distributions.')

    def conjugate(self, other_distribution, category_frequencies):
        if isinstance(other_distribution, Binomial):
            parameters = self.get_concrete_parameters()
            for category, frequency in category_frequencies.iteritems():
                parameters[category] += frequency
            conjugate = Beta(parameters)
        else:
            raise TypeError('Conjugacy for Beta only implemented with Binomial.')

        return conjugate

    def __str__(self):
        return 'Beta({};{})'.format(self.a, self.b)

    def __repr__(self):
        return self.__str__()

if __name__ == '__main__':
    from distribution.continuous.beta import Beta
    from distribution.discrete.binomial import Binomial
    from base.node_metadata import NodeMetadata
    from base.edge_metadata import EdgeMetadata
    from base.node import Node
    from distribution.cpd import CPD
    from model.pgm_metadata import PGMMetadata
    from model.pgm import PGM
    import numpy as np

    pi = NodeMetadata('pi')
    node = NodeMetadata('n')
    nodes = [pi, node]
    edges = [EdgeMetadata(pi, node)]

    cpd_pi = CPD(pi, [], Beta(1, 1))
    cpd_node = CPD(node, [pi], Binomial(Node(pi)))
    cpds = [cpd_pi, cpd_node]

    pgm_meta = PGMMetadata()
    pgm_meta.add_nodes_from(nodes)
    pgm_meta.add_edges_from(edges)
    pgm_meta.add_cpds_from(cpds)

    pgm = PGM(pgm_meta)

    d1 = pgm.nodes(data='data')[('pi', 0)].cpd.get_distribution()
    d2 = pgm.nodes(data='data')[('n', 0)].cpd.get_distribution()
    d3 = d1.mult(d2, 0)

    samples = []

    for i in range(100):
        samples.append(d3.sample())

    print(np.mean(samples))
