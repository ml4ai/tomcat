from distribution.distribution import Distribution
from scipy.stats import norm
from base.node import Node
import numpy as np

class Gaussian(Distribution):

    """
    Class that represents a Gaussian distribution 
    """

    def __init__(self, mean, variance):
        self.mean = mean
        self.variance = variance

    def sample(self):
        mean, variance = self.get_concrete_parameters()
        return norm(mean, variance).rvs()

    def get_concrete_parameters(self):
        """
        Retrieves the parameters.
        """
        
        if isinstance(self.mean, Node):
            mean = self.mean.assignment
        else:
            mean = self.mean

        if isinstance(self.variance, Node):
            variance = self.variance.assignment
        else:
            variance = self.variance

        return mean, variance

    def get_probability(self, values_and_frequencies, log_transform=False):
        if values_and_frequencies.empty:
            return 0
        else:
            mean, variance = self.get_concrete_parameters()
            density = 0 if log_transform else 1

            for category, frequency in values_and_frequencies.iteritems():
                if log_transform:
                    density += np.log(norm(mean, variance).pdf(category)) * frequency
                else:
                    density *= norm(mean, variance).pdf(category) ** frequency

        return density

    def replace_parameter_node(self, node):
        """
        Replaces a parameter node by another of equivalent metadata
        """
        if isinstance(self.mean, Node) and self.mean.metadata == node.metadata:
            self.mean = node

        if isinstance(self.variance, Node) and self.variance.metadata == node.metadata:
            self.variance = node

    def mult(self, other_distribution):
        raise TypeError('Multiplication not yet implemented for Gaussian distributions.')

    def power(self, exponent):
        raise TypeError('Power not yet implemented for Gaussian distributions.')

    def conjugate(self, other_distribution, category_frequencies):
        raise TypeError('Conjugacy not implemented for Gaussian distributions.')

    def depends_on(self, node):
        return self.mean == node or self.variance == node

    def fill_parameters(self, parameter_values):
        """
        This function replaces node assignment in the distribution by actual values
        """
        if isinstance(self.mean, Node) and self.mean.metadata.label in parameter_values.index:
            self.mean = parameter_values[self.mean.metadata.label]

        if isinstance(self.variance, Node) and self.variance.metadata.label in parameter_values.index:
            self.variance = parameter_values[self.variance.metadata.label]

    def __str__(self):
        return 'Gaussian({};{})'.format(self.mean, self.variance)

    def __repr__(self):
        return self.__str__()

if __name__ == '__main__':
    pass

    # Test conjugacy
