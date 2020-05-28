from .distribution import Distribution
from scipy.stats import norm, multivariate_normal
import numpy as np

class Gaussian(Distribution):

    """
    Class that represents a Gaussian distribution (either multivariate or univariate)
    """

    def __init__(self, means, covariance):
        if isinstance(means, list) and len(means) > 1:
            self.d = multivariate_normal(means, covariance)                         
        else:
            self.d = norm(means, covariance)             

    def sample(self):
        return self.d.rvs()

    def get_probability(self, states):
        """
        Retrieves the joint density of a given set of states
        """
        return self.d.pdf(states)
