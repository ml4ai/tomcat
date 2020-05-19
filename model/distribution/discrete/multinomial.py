import numpy as np
from ..distribution import Distribution

class Multinomial(Distribution):

    """
    Class that represents a Multinomial distribution
    """

    def __init__(self, probabilities):
        self.probabilities = np.array(probabilities)

    def sample(self):
        return np.random.choice(range(len(self.probabilities)), p=self.probabilities)

    def get_probability(self, states):
        """
        Retrieves the probability of a given state. If multiple states are informed, the probability is zero
        since only one can occur at a time.
        """
        if isinstance(states, list) and len(states) > 1:
            return 0
        else:
            return self.probabilities[states]

    def __add__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.probabilities + pmf.probabilities 
        else:
            return self.probabilities + pmf

    def __mul__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.probabilities * pmf.probabilities 
        else:
            return self.probabilities * pmf

    def __floordiv__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.probabilities / pmf.probabilities 
        else:
            return self.probabilities / pmf

    def __truediv__(self, pmf):
        return self.__floordiv__(pmf) 