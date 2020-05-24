from ..distribution import Distribution
from scipy.stats import dirichlet

class Dirichlet(Distribution):

    """
    Class that represents a Dirichlet distribution
    """

    def __init__(self, alphas):
        self.dirichlet = dirichlet(alphas)         

    def sample(self):
        return self.dirichlet.rvs()

    def get_probability(self, states):
        """
        Retrieves the joint density of a given set of states
        """
        return self.dirichlet.pdf(states)
