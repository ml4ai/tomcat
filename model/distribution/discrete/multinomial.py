import numpy as np
from ..distribution import Distribution
from base.node import Node

class Multinomial(Distribution):

    """
    Class that represents a Multinomial distribution
    """

    def __init__(self, probabilities):
        self.probabilities = np.array(probabilities)

    def sample(self):
        probabilities = self.get_probabilities()

        return np.random.choice(range(len(probabilities)), p=probabilities)

    def get_probabilities(self):
        """
        Retrieves the probabilities. 
        """
        
        return [probability.assignment if isinstance(probability, Node)
                else probability
                for probability in self.probabilities]  

    def get_probability(self, state):
        """
        Retrieves the probability of a given state. 
        """
        
        if isinstance(self.probabilities[state], Node):
            return self.probabilities[state].assignment
        else:
            return self.probabilities[state]

    def __str__(self):
        return 'Mult({})'.format(self.probabilities)

    def __repr__(self):
        return self.__str__()

    def __add__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.get_probabilities() + pmf.get_probabilities() 
        else:
            return self.get_probabilities() + pmf

    def __mul__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.get_probabilities() * pmf.get_probabilities() 
        else:
            return self.get_probabilities() * pmf

    def __floordiv__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.get_probabilities() / pmf.get_probabilities() 
        else:
            return self.get_probabilities() / pmf

    def __truediv__(self, pmf):
        return self.__floordiv__(pmf)  