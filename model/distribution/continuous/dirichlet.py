from distribution.distribution import Distribution
from distribution.discrete.multinomial import Multinomial
from scipy.stats import dirichlet
from base.node import Node


class Dirichlet(Distribution):
    """
    Class that represents a Dirichlet distribution
    """

    def __init__(self, alphas, f_alphas=[]):
        self.alphas = alphas
        self.f_alphas = f_alphas

        if isinstance(self.alphas, Node):
            dimensionality = self.alphas.metadata.cardinality
        else:
            dimensionality = len(self.alphas)

        if self.f_alphas == []:
            self.f_alphas = [lambda x: x] * dimensionality

    def sample(self):
        alphas = self.get_concrete_parameters()
        return dirichlet(alphas).rvs()[0]

    def get_concrete_parameters(self):
        """
        Retrieves the parameters.
        """

        if isinstance(self.alphas, Node):
            alphas = [self.f_alphas[i](alpha) for i, alpha in enumerate(self.alphas.assignment)]
        else:
            alphas = [self.f_alphas[i](alpha) for i, alpha in enumerate(self.alphas)]

        return alphas

    def get_probability(self, states):
        """
        Retrieves the joint density of a given set of states
        """
        alphas = self.get_concrete_parameters()
        return dirichlet(alphas).pdf(states)

    def mult(self, other_distribution, states_counts, self_state=None):
        if isinstance(other_distribution, Multinomial):
            if isinstance(other_distribution.probabilities, Node):
                if any(d == self for d in other_distribution.probabilities.cpd.values):
                    f_alphas = self.f_alphas.copy()

                    # todo - python only replaces the variables upon calling the function so I don't know how to
                    #        use lambda for this case. I'll change the alpha directly to move on since I don't
                    #        need to apply any function to a Dirichlet at this point

                    # for state, occurences in states_counts.items():
                    #     f_alpha = f_alphas[state]
                    #     f_alphas[state] = (lambda f: (lambda o: lambda x: (f(x) + o))(occurences))(f_alpha)
                    alphas = self.alphas.copy()
                    for state, occurences in states_counts.items():
                        alphas[state] += occurences

                    composite_distribution = Dirichlet(alphas, f_alphas)
                else:
                    raise TypeError(
                        'There no dependency between {} and the parameters of {}'.format(self, other_distribution))
            else:
                raise TypeError(
                    'There no dependency between {} and the parameters of {}'.format(self, other_distribution))
        else:
            raise TypeError(
                'Not compatible distributions. {} and {} are not conjugates.'.format(self, other_distribution))

        return composite_distribution

    def __str__(self):
        return '[a]Dirichlet({})\n[c]Dirichlet({})'.format(self.alphas, self.get_concrete_parameters())

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
