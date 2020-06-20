class Distribution:

    """
    Class that represents an abstract definition of a distribution
    """
    
    def sample(self):
        """
        Defined in a concrete implementation of a distribution
        """
        pass

    def get_probability(self, states):
        """
        Defined in a concrete implementation of a distribution
        """
        pass