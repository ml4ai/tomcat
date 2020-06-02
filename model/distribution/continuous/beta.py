from distribution.distribution import Distribution
from scipy.stats import beta
from base.node import Node
from distribution.discrete.binomial import Binomial
import copy


class Beta(Distribution):
    """
    Class that represents a Beta distribution
    """

    def __init__(self, a, b, f_a=lambda x: x, f_b=lambda x: x):
        self.a = a
        self.b = b
        self.f_a = f_a
        self.f_b = f_b

    def sample(self):
        a, b = self.get_concrete_parameters()
        return beta(a, b).rvs()

    def get_concrete_parameters(self):
        """
        Retrieves the parameters.
        """

        if isinstance(self.a, Node):
            a = self.f_a(self.a.assignment)
        else:
            a = self.f_a(self.a)

        if isinstance(self.b, Node):
            b = self.f_b(self.b.assignment)
        else:
            b = self.f_b(self.b)

        return a, b

    def get_probability(self, state):
        """
        Retrieves the density of a given state
        """
        a, b = self.get_concrete_parameters()
        return beta(a, b).pdf(state)

    def mult(self, other_distribution, states_counts, self_state=None):
        if isinstance(other_distribution, Binomial):
            if isinstance(other_distribution.p, Node):
                if any(d == self for d in other_distribution.p.cpd.values):
                    occurences_0 = 0
                    if 0 in states_counts.index:
                        occurences_0 = states_counts[0]

                    occurences_1 = 0
                    if 1 in states_counts.index:
                        occurences_1 = states_counts[1]

                    f_a = lambda x : self.f_a(x) + occurences_1
                    f_b = lambda x: self.f_b(x) + occurences_0
                    composite_distribution = Beta(self.a, self.b, f_a, f_b)
                else:
                    raise TypeError(
                        'There\'s no dependency between {} and the parameters of {}'.format(self, other_distribution))
            else:
                raise TypeError(
                    'There\'s no dependency between {} and the parameters of {}'.format(self, other_distribution))
        else:
            raise TypeError(
                'Not compatible distributions. {} and {} are not conjugates.'.format(self, other_distribution))

        return composite_distribution

    def __str__(self):
        return '[a]Beta({};{})\n[c]Beta({};{})'.format(self.a, self.b, *self.get_concrete_parameters())

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
