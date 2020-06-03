from distribution.distribution import Distribution
from distribution.discrete.multinomial import Multinomial
from scipy.stats import dirichlet
from base.node import Node


class Dirichlet(Distribution):
    """
    Class that represents a Dirichlet distribution
    """

    def __init__(self, alphas):
        self.alphas = alphas

    def sample(self):
        alphas = self.get_concrete_parameters()
        return dirichlet(alphas).rvs()[0]

    def get_concrete_parameters(self):
        """
        Retrieves the parameters.
        """

        if isinstance(self.alphas, Node):
            alphas = self.alphas.assignment
        else:
            alphas = self.alphas

        return alphas

    def get_probability(self, category_frequencies):
        """
        Retrieves the density of a given state
        """
        alphas = self.get_concrete_parameters()
        density = 1
        for category, frequency in category_frequencies.iteritems():
            density *= dirichlet(alphas).pdf(category)**frequency

        return density

    def mult(self, other_distribution):
        raise TypeError('Multiplication not yet implemented for Dirichlet distributions.')

    def power(self, exponent):
        raise TypeError('Power not yet implemented for Dirichlet distributions.')

    def conjugate(self, other_distribution, category_frequencies):
        if isinstance(other_distribution, Multinomial):
            parameters = self.get_concrete_parameters()
            for category, frequency in category_frequencies.iteritems():
                parameters[category] += frequency
            conjugate = Dirichlet(parameters)
        else:
            raise TypeError('Conjugacy for Dirichlet only implemented with Multinomial.')

        return conjugate

    def __str__(self):
        return 'Dirichlet({})'.format(self.alphas)

    def __repr__(self):
        return self.__str__()

    if __name__ == '__main__':
        from distribution.continuous.dirichlet import Dirichlet
        from distribution.discrete.multinomial import Multinomial
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

        cpd_pi = CPD(pi, [], Dirichlet([1, 1, 1]))
        cpd_node = CPD(node, [pi], Multinomial(Node(pi)))
        cpds = [cpd_pi, cpd_node]

        pgm_meta = PGMMetadata()
        pgm_meta.add_nodes_from(nodes)
        pgm_meta.add_edges_from(edges)
        pgm_meta.add_cpds_from(cpds)

        pgm = PGM(pgm_meta)

        d1 = pgm.nodes(data='data')[('pi', 0)].cpd.get_distribution()
        d2 = pgm.nodes(data='data')[('n', 0)].cpd.get_distribution()
        d3 = d1.mult(d2, 2)

        samples = np.array([[0, 0, 0]])

        for i in range(100):
            samples = np.append(samples, [d3.sample()], axis=0)

        print(np.mean(samples, axis=0))
