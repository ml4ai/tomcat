from ..distribution import Distribution
from scipy.stats import norm, multivariate_normal

class Gaussian(Distribution):

    """
    Class that represents a Gaussian distribution (either multivariate or univariate)
    """

    def __init__(self, means, covariance):
        if isinstance(means, list) and len(means) > 1:
            self.normal = multivariate_normal(means, covariance)                         
        else:
            self.normal = norm(means, covariance)             

    def sample(self):
        return self.normal.rvs()

    def get_probability(self, states):
        """
        Retrieves the joint density of a given set of states
        """
        return self.normal.pdf(states)
