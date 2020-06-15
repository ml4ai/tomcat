from distribution.distribution import Distribution
from scipy.stats import invgamma
import numpy as np
from base.node import Node
from distribution.continuous.gaussian import Gaussian


class InverseGamma(Distribution):
    """
    Class that represents a Beta distribution
    """

    def __init__(self, a, b):
        self.a = a
        self.b = b

    def sample(self):
        a, b = self.get_concrete_parameters()
        return invgamma(a, b).rvs()

    def get_concrete_parameters(self):
        """
        Retrieves the parameters.
        """

        if isinstance(self.a, Node):
            a = self.a.assignment
        else:
            a = self.a

        if isinstance(self.b, Node):
            b = self.b.assignment
        else:
            b = self.b

        return a, b

    def get_probability(self, values_and_frequencies, log_transform=False):
        """
        Retrieves the density of a given state
        """

        if values_and_frequencies.empty:
            return 0
        else:
            a, b = self.get_concrete_parameters()
            density = 0 if log_transform else 1

            for category, frequency in values_and_frequencies.iteritems():
                if log_transform:
                    density += np.log(invgamma(a, b).pdf(category)) * frequency
                else:
                    density *= invgamma(a, b).pdf(category) ** frequency

        return density

    def mult(self, other_distribution):
        raise TypeError('Multiplication not yet implemented for Inverse Gamma distributions.')

    def power(self, exponent):
        raise TypeError('Power not yet implemented for Inverse Gamma distributions.')

    def conjugate(self, other_distribution, category_frequencies):
        if isinstance(other_distribution, Gaussian):
            a, b = self.get_concrete_parameters()
            n = 0
            for category, frequency in category_frequencies.iteritems():
                n += frequency * 1
                mean, _ = other_distribution.get_concrete_parameters()
                b += frequency*((category - mean)**2)

            a += n/2
            conjugate = InverseGamma(a, b)
        else:
            raise TypeError('Conjugacy for Beta only implemented with Binomial.')

        return conjugate

    def depends_on(self, node):
        return self.a == node or self.b == node

    def fill_parameters(self, parameter_values):
        """
        This function replaces node assignment in the distribution by actual values
        """
        if isinstance(self.a, Node) and self.a.metadata.label in parameter_values.index:
            self.a = parameter_values[self.a.metadata.label]

        if isinstance(self.b, Node) and self.b.metadata.label in parameter_values.index:
            self.b = parameter_values[self.b.metadata.label]

    def __str__(self):
        return 'InverseGamma({};{})'.format(self.a, self.b)

    def __repr__(self):
        return self.__str__()


