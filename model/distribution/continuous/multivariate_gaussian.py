from ..distribution import Distribution
from scipy.stats import norm, multivariate_normal
from base.node import Node
import numpy as np

class MultivariateGaussian(Distribution):

    """
    Class that represents a Multivariate Gaussian distribution
    """

    def __init__(self, means, covariance=[], variances=[], transformations_mean=[], transformations_variance=[]):
        self.means = means
        self.covariance = covariance
        self.variances = variances

        if covariance != [] and variances != []:
            raise TypeError('Either covariance or variances has to be empty')

        if transformations_mean == []:
            self.transformations_mean = [lambda x: x for _ in self.means]
        else:
            self.transformations_mean = transformations_mean

        if transformations_variance == []:
            self.transformations_variance = [lambda x: x for _ in self.variances]       
        else:
            self.transformations_variance = transformations_variance

    def sample(self):
        means, covariance = self.get_concrete_parameters()
        normal = multivariate_normal(means, covariance)                         
        return normal.rvs()

    def get_concrete_parameters(self):
        """
        Retrieves the parameters. 
        """

        means = []        
        for i, mean in enumerate(self.means):
            if isinstance(mean, Node):
                means.append(self.transformations_mean[i](mean.assignment))
            else:
                means.append(self.transformations_mean[i](mean))

        if self.covariance == []:
            variances = []
            for i, variance in enumerate(self.variances):
                if isinstance(variance, Node):
                    variances.append(self.transformations_variance[i](variance.assignment))
                else:
                    variances.append(self.transformations_variance[i](variance))

            dim = len(variances)
            covariance = np.zeros([dim, dim])
            np.fill_diagonal(covariance, variances)
        else:
            covariance = self.covariance 

        return means, covariance

    def get_probability(self, states):
        """
        Retrieves the joint density of a given set of states
        """
        means, covariance = self.get_concrete_parameters()
        normal = multivariate_normal(means, covariance)      
        print(means, covariance)                      
        return normal.pdf(states)

    def replace_parameter_node(self, node):
        """
        Replaces a parameter node by another of equivalent metadata
        """
        for i, mean in enumerate(self.means):
            if isinstance(mean, Node) and mean.metadata == node.metadata:
                self.means[i] = node

        for i, variance in enumerate(self.variances):
            if isinstance(variance, Node) and variance.metadata == node.metadata:
                self.variances[i] = node

    def __str__(self):
        if self.covariance = []:
            return 'MultGaussian({};{})'.format(self.means, self.variances)
        else:
            return 'MultGaussian({};{})'.format(self.means, self.covariance)

    def __repr__(self):
        return self.__str__()
