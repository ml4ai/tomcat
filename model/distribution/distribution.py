import numpy as np

class Distribution:

    """
    Class that represents an abstract definition of a distribution
    """

    DIGITS_PRECISION = -16 * np.log(10)
    LOG_0 = 10**-16
    
    def sample(self):
        """
        Defined in a concrete implementation of a distribution
        Generates a new sample
        """
        pass

    def get_probability(self, states):
        """
        Defined in a concrete implementation of a distribution.
        Retrieves the probability or density for a given state/value
        """
        pass

    def replace_parameter_node(self, node):
        """
        Defined in a concrete implementation of a distribution.
        Replaces a parameter node by another of equivalent metadata
        """
        pass