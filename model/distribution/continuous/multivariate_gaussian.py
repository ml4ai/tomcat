from distribution.distribution import Distribution
from scipy.stats import multivariate_normal
from base.node import Node
import numpy as np

class MultivariateGaussian(Distribution):

    """
    Class that represents a Multivariate Gaussian distribution
    """

    def __init__(self, means, covariance=[], variances=[], f_means=[], f_variances=[]):
        self.means = means
        self.covariance = covariance
        self.variances = variances
        self.f_means = f_means
        self.f_variances = f_variances

        if covariance != [] and variances != []:
            raise TypeError('Either covariance or variances has to be empty')

        if isinstance(self.means, Node):
            dimensionality = self.means.metadata.dimensionality
        else:
            dimensionality = len(self.means)

        if self.f_means == []:
            self.f_mean = [lambda x: x]*dimensionality

        if self.f_variances == []:
            self.f_variances = [lambda x: x]*self.dimensionality

    def sample(self):
        means, covariance = self.get_concrete_parameters()
        return multivariate_normal(means, covariance).rvs()

    def get_concrete_parameters(self):
        """
        Retrieves the parameters. 
        """

        means = []        
        for i, mean in enumerate(self.means):
            if isinstance(mean, Node):
                means.append(self.f_mean[i](mean.assignment))
            else:
                means.append(self.f_mean[i](mean))

        if self.covariance == []:
            variances = []
            for i, variance in enumerate(self.variances):
                if isinstance(variance, Node):
                    variances.append(self.f_variances[i](variance.assignment))
                else:
                    variances.append(self.f_variances[i](variance))

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
        return multivariate_normal(means, covariance).pdf(states)

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
        if self.covariance == []:
            return 'MultGaussian({};{})'.format(self.means, self.variances)
        else:
            return 'MultGaussian({};{})'.format(self.means, self.covariance)

    def __repr__(self):
        return self.__str__()
