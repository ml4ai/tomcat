class Distribution:

    """
    Class that represents an abstract definition of a distribution
    """
    
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