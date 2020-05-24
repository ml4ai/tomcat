from ..distribution import Distribution
from scipy.stats import beta

class Beta(Distribution):

    """
    Class that represents a Beta distribution
    """

    def __init__(self, alpha, beta):
        self.beta = beta(alpha, beta)         

    def sample(self):
        return self.beta.rvs()

    def get_probability(self, state):
        """
        Retrieves the density of a given state
        """
        return self.beta.pdf(state)
