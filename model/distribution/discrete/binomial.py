import numpy as np
from ..distribution import Distribution
from base.node import Node
import copy

class Binomial(Distribution):

    """
    Class that represents a Binomial distribution
    """

    def __init__(self, p):
        if isinstance(p, Node):
            self.node = p
            self.probabilities = []
        else:
            self.probabilities = np.array([1-p, p])
            self.node = None

    def sample(self):
        probabilities = self.get_probabilities()            
        return np.random.choice([0,1], p=probabilities)

    def get_probabilities(self):
        """
        Retrieves the probabilities. 
        """

        if self.node == None:
            probabilities = self.probabilities.copy()            
        else:
            probabilities = [1 - self.node.assignment, self.node.assignment]

        return probabilities

    def get_probability(self, state):
        """
        Retrieves the probability of a given state.
        """
        probabilities = self.get_probabilities()
        return probabilities[state]        

    def __str__(self):
        return 'Binomial({})'.format(self.probabilities)

    def __repr__(self):
        return self.__str__()

    def __add__(self, pmf): 
        if isinstance(pmf, Binomial):
            return self.get_probabilities() + pmf.get_probabilities() 
        else:
            return self.get_probabilities() + pmf

    def __mul__(self, pmf): 
        if isinstance(pmf, Binomial):
            return self.get_probabilities() * pmf.get_probabilities() 
        else:
            return self.get_probabilities() * pmf

    def __floordiv__(self, pmf): 
        if isinstance(pmf, Binomial):
            return self.get_probabilities() / pmf.get_probabilities() 
        else:
            return self.get_probabilities() / pmf

    def __truediv__(self, pmf):
        return self.__floordiv__(pmf) 