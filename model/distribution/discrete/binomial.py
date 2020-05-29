import numpy as np
from distribution.distribution import Distribution
from base.node import Node
import copy

class Binomial(Distribution):

    """
    Class that represents a Binomial distribution
    """

    def __init__(self, p, f_ps=[]):
        self.p = p   
        self.f_ps = f_ps

        if f_ps == []:
            self.f_ps = [lambda x : x]*2

    def sample(self):
        probabilities = self.get_concrete_probabilities()
        return np.random.choice([0,1], p=probabilities)

    def get_concrete_probabilities(self):
        if isinstance(self.p, Node):
            probabilities = self.p.assignment
        else:
            probabilities = self.p

        if not isinstance(probabilities, list):
            probabilities = [probabilities]

        if len(probabilities) == 1:
            probabilities.insert(0, 1 - probabilities[-1])

        probabilities = [self.f_ps[i](p) for i, p in enumerate(probabilities)]
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
        if isinstance(self.p, Node) and self.p.metadata == node.metadata:
            self.p = node

    def mult(self, other_distribution, state, self_state):
        f_ps = self.f_ps.copy()
        f_p = f_ps[self_state]
        p = other_distribution.get_probability(state)
        f_ps[self_state] = lambda x : f_p(x)*p

        composite_distribution = Binomial(self.p, f_ps)
        return composite_distribution

    def __str__(self):
        if isinstance(self.p, Node):
            return 'Binomial([1-{} {}])'.format(self.p, self.p)
        else:
            return 'Binomial({})'.format(self.probabilities)

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