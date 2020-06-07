import numpy as np
from distribution.distribution import Distribution
from base.node import Node
import shlex

class Multinomial(Distribution):

    """
    Class that represents a Multinomial distribution
    """

    def __init__(self, probabilities, in_log_scale=False):
        self.probabilities = probabilities
        self.in_log_scale = in_log_scale

    def sample(self):
        probabilities = self.get_concrete_probabilities()
        return np.random.choice(range(len(probabilities)), p=probabilities)

    def get_concrete_probabilities(self, normalize=True, log_transform=False):
        """
        Retrieves the probabilities. 
        """

        if isinstance(self.probabilities, Node):
            probabilities = self.probabilities.assignment
        else:
            probabilities = self.probabilities

        if not self.in_log_scale:
            probabilities = [Distribution.LOG_0 if p==0 else p for p in probabilities]
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
        """
        Retrieves the probability of a given state. 
        """

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
        if isinstance(self.probabilities, Node) and self.probabilities.metadata == node.metadata:
            self.probabilities = node

    def mult(self, other_distribution, in_log_scale=False):
        if isinstance(other_distribution, list):
            if in_log_scale:
                log_probabilities = np.array(other_distribution)
            else:
                log_probabilities = np.log(np.array(other_distribution))
        elif isinstance(other_distribution, Multinomial):
            log_probabilities = other_distribution.get_concrete_probabilities(normalize=False, log_transform=True)
        else:
            raise TypeError('Cannot multiply a Multinomial distribution by {}.'.format(other_distribution))

        log_probabilities = self.get_concrete_probabilities(normalize=False, log_transform=True) + log_probabilities
        return Multinomial(log_probabilities, in_log_scale=True)

    def power(self, exponent):
        log_probabilities = self.get_concrete_probabilities(normalize=False, log_transform=True) * exponent
        return Multinomial(log_probabilities, in_log_scale=True)

    def conjugate(self, other_distribution, category_frequencies):
        raise TypeError('Conjugacy not implemented for Multinomial distributions.')

    def depends_on(self, node):
        return self.probabilities == node

    def fill_parameters(self, parameter_values):
        """
        This function replaces node assignment in the distribution by actual values
        """
        if isinstance(self.probabilities, Node) and self.probabilities.metadata.label in parameter_values.index:
            self.probabilities = parameter_values[self.probabilities.metadata.label]

    def __str__(self):
        return 'Multinomial({})'.format(self.probabilities)

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

    pi = NodeMetadata('pi', cardinality=3)
    node = NodeMetadata('n', cardinality=2)
    nodes = [pi, node]
    edges = [EdgeMetadata(pi, node)]

    cpd_pi = CPD(pi, [], Multinomial([0.2, 0.5, 0.3]))
    cpd_node = CPD(node, [pi], [Binomial(0.3), Binomial(0.2), Binomial(0.1)])
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

    #print(np.mean(samples))