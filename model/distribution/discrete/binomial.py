import numpy as np
from distribution.distribution import Distribution
from base.node import Node
import copy

class Binomial(Distribution):

    """
    Class that represents a Binomial distribution
    """

    def __init__(self, p, in_log_scale=False):
        self.p = p
        self.in_log_scale = in_log_scale

    def sample(self):
        probabilities = self.get_concrete_probabilities()
        return np.random.choice([0,1], p=probabilities)

    def get_concrete_probabilities(self, normalize=True, log_transform=False):
        if isinstance(self.p, Node):
            probabilities = self.p.assignment
        else:
            probabilities = self.p

        if not isinstance(probabilities, list):
            probabilities = [probabilities]

        if len(probabilities) == 1:
            probabilities.insert(0, 1 - probabilities[-1])

        if not self.in_log_scale:
            probabilities = np.log(probabilities)

        if normalize:
            probabilities = self.normalize(probabilities)
            probabilities = np.log(probabilities)

        if not log_transform:
            probabilities = np.exp(probabilities)

        return probabilities

    def normalize(self, log_probabilities):
        precision = Distribution.DIGITS_PRECISION - np.log(len(log_probabilities))
        max_value = np.max(log_probabilities)
        log_normalized_probabilities = log_probabilities - max_value
        log_normalized_probabilities = [0 if log_prob < precision else log_prob for log_prob in log_normalized_probabilities]
        normalized_probabilities = np.exp(log_normalized_probabilities)
        normalized_probabilities /= np.sum(normalized_probabilities)

        return normalized_probabilities

    def get_probability(self, category_frequencies, log_transform=False):
        if category_frequencies.empty:
            return 0
        else:
            probabilities = self.get_concrete_probabilities(log_transform=log_transform)
            probability = 0 if log_transform else 1

            for category, frequency in category_frequencies.iteritems():
                if log_transform:
                    probability += probabilities[category] * frequency
                else:
                    probability *= probabilities[category] ** frequency

        return probability

    def replace_parameter_node(self, node):
        """
        Replaces a parameter node by another of equivalent metadata
        """
        if isinstance(self.p, Node) and self.p.metadata == node.metadata:
            self.p = node

    def mult(self, other_distribution, in_log_scale=False):
        if isinstance(other_distribution, list):
            if in_log_scale:
                log_probabilities = np.array(other_distribution)
            else:
                log_probabilities = np.log(np.array(other_distribution))
        elif isinstance(other_distribution, Binomial):
            log_probabilities = other_distribution.get_concrete_probabilities(normalize=False, log_transform=True)
        else:
            raise TypeError('Cannot multiply a Binomial distribution by {}.'.format(other_distribution))

        log_probabilities = self.get_concrete_probabilities(normalize=False, log_transform=True) + log_probabilities
        return Binomial(log_probabilities, in_log_scale=True)

    def power(self, exponent):
        log_probabilities = self.get_concrete_probabilities(normalize=False, log_transform=True) * exponent
        return Binomial(log_probabilities, in_log_scale=True)

    def conjugate(self, other_distribution, category_frequencies):
        raise TypeError('Conjugacy not implemented for Binomial distributions.')

    def depends_on(self, node):
        return self.p == node

    def fill_parameters(self, parameter_values):
        """
        This function replaces node assignment in the distribution by actual values
        """
        if isinstance(self.p, Node) and self.p.metadata.label in parameter_values.index:
            self.p = parameter_values[self.p.metadata.label]

    def __str__(self):
        if isinstance(self.p, Node):
            return 'Binomial([1-{}, {}])'.format(self.p, self.p)
        else:
            return 'Binomial({})'.format(self.get_concrete_probabilities(normalize=False))

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