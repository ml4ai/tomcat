import numpy as np
from distribution.distribution import Distribution
from base.node import Node
import copy

class Binomial(Distribution):

    """
    Class that represents a Binomial distribution
    """

    def __init__(self, p):
        self.p = p

    def sample(self):
        probabilities = self.get_concrete_probabilities()
        return np.random.choice([0,1], p=probabilities)

    def get_concrete_probabilities(self, normalize=True):
        if isinstance(self.p, Node):
            probabilities = self.p.assignment
        else:
            probabilities = self.p

        if not isinstance(probabilities, list):
            probabilities = [probabilities]

        if len(probabilities) == 1:
            probabilities.insert(0, 1 - probabilities[-1])

        if normalize:
            probabilities /= np.sum(probabilities)
        return probabilities

    def get_probability(self, category_frequencies):
        probabilities = self.get_concrete_probabilities()
        probability = None
        for category, frequency in category_frequencies.iteritems():
            if probability == None:
                probability = probabilities[category] ** frequency
            else:
                probability *= probabilities[category] ** frequency

        if probability == None:
            probability = 0

        return probability

    def replace_parameter_node(self, node):
        """
        Replaces a parameter node by another of equivalent metadata
        """
        if isinstance(self.p, Node) and self.p.metadata == node.metadata:
            self.p = node

    def mult(self, other_distribution):
        if isinstance(other_distribution, list):
            probabilities = np.array(other_distribution)
        elif isinstance(other_distribution, Binomial):
            probabilities = other_distribution.get_concrete_probabilities(normalize=False)
        else:
            raise TypeError('Cannot multiply a Binomial distribution by {}.'.format(other_distribution))

        return Binomial(self.get_concrete_probabilities(normalize=False) * probabilities)

    def power(self, exponent):
        return Binomial(self.get_concrete_probabilities(normalize=False) ** exponent)

    def __str__(self):
        if isinstance(self.p, Node):
            return 'Binomial([1-{} {}])'.format(self.p, self.p)
        else:
            return 'Binomial({})'.format(self.get_concrete_probabilities(normalize=False))

    def conjugate(self, other_distribution, category_frequencies):
        raise TypeError('Conjugacy not implemented for Binomial distributions.')

    def __repr__(self):
        return self.__str__()

if __name__ == '__main__':
    from distribution.discrete.multinomial import Multinomial
    from distribution.discrete.binomial import Binomial
    from base.node_metadata import NodeMetadata
    from base.edge_metadata import EdgeMetadata
    from base.node import Node
    from distribution.cpd import CPD
    from model.pgm_metadata import PGMMetadata
    from model.pgm import PGM
    import numpy as np

    pi = NodeMetadata('pi', cardinality=2)
    node = NodeMetadata('n', cardinality=3)
    nodes = [pi, node]
    edges = [EdgeMetadata(pi, node)]

    cpd_pi = CPD(pi, [], Binomial([0.2]))
    cpd_node = CPD(node, [pi], [Multinomial([0.3, 0.5, 0.2]), Multinomial([0.1, 0.6, 0.3])])
    cpds = [cpd_pi, cpd_node]

    pgm_meta = PGMMetadata()
    pgm_meta.add_nodes_from(nodes)
    pgm_meta.add_edges_from(edges)
    pgm_meta.add_cpds_from(cpds)

    pgm = PGM(pgm_meta)

    d1 = pgm.nodes(data='data')[('pi', 0)].cpd.get_distribution()
    for i, d in enumerate(pgm.nodes(data='data')[('n', 0)].cpd.get_distribution()):
        d1 = d1.mult(d, 0, i)

    samples = []

    for i in range(100):
        samples.append(d1.sample())

    print(np.mean(samples))