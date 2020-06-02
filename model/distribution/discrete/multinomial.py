import numpy as np
from distribution.distribution import Distribution
from base.node import Node

class Multinomial(Distribution):

    """
    Class that represents a Multinomial distribution
    """

    def __init__(self, probabilities, f_probabilities=[]):
        self.probabilities = probabilities
        self.f_probabilities = f_probabilities

        if isinstance(self.probabilities, Node):
            self.dimensionality = self.probabilities.metadata.dimensionality
        else:
            self.dimensionality = len(self.probabilities)

        if self.f_probabilities == []:
            self.f_probabilities = [lambda x : x]*self.dimensionality

    def sample(self):
        probabilities = self.get_concrete_probabilities()
        return np.random.choice(range(len(probabilities)), p=probabilities)

    def get_concrete_probabilities(self):
        """
        Retrieves the probabilities. 
        """

        if isinstance(self.probabilities, Node):
            probabilities = [self.f_probabilities[i](probability) for i, probability in enumerate(self.probabilities.assignment)]
        else:
            probabilities = [self.f_probabilities[i](probability) for i, probability in enumerate(self.probabilities)]

        probabilities = probabilities / np.sum(probabilities)
        return probabilities

    def get_probability(self, state):
        """
        Retrieves the probability of a given state. 
        """
        return self.get_concrete_probabilities()[int(state)]

    def replace_parameter_node(self, node):
        """
        Replaces a parameter node by another of equivalent metadata
        """
        if isinstance(self.probabilities, Node) and self.probabilities.metadata == node.metadata:
            self.probabilities = node

    def mult(self, other_distribution, states_counts, self_state):
        f_probabilities = self.f_probabilities.copy()
        f_probability = f_probabilities[self_state]
        p = 1
        for state, occurences in states_counts.items():
            p *= other_distribution.get_probability(state)
            p = p ** occurences
        f_probabilities[self_state] = lambda x : f_probability(x)*p

        composite_distribution = Multinomial(self.probabilities, f_probabilities)
        return composite_distribution

    def add(self, other_multinomial):
        probabilities = self.get_concrete_probabilities() + other_multinomial.get_concrete_probabilities()
        return Multinomial(probabilities)

    def __str__(self):
        return 'Mult({})'.format(self.probabilities)

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