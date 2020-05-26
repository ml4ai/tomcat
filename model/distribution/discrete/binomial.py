import numpy as np
from ..distribution import Distribution
from base.node import Node
import copy

class Binomial(Distribution):

    """
    Class that represents a Binomial distribution
    """

    def __init__(self, p, transformation=lambda x: x):        
        self.p = p   
        self.transformation = transformation     

    def sample(self):
        probabilities = self.get_concrete_probabilities()
        return np.random.choice([0,1], p=probabilities)

    def get_concrete_probabilities(self):
        probabilities = [0, 0]

        if isinstance(self.p, Node):
            probabilities[1] = self.transformation(self.p.assignment)
        else:
            probabilities[1] = self.transformation(self.p)

        probabilities[0] = 1 - probabilities[1]
        return np.array(probabilities)

    def get_probability(self, state):
        """
        Retrieves the probability of a given state.
        """
        return self.get_concrete_probabilities()[state] 

    def replace_parameter_node(self, node):
        """
        Replaces a parameter node by another of equivalent metadata
        """
        if isinstance(self.p, Node) and self.p.metadata == node.metadata:
            self.p = node

    def __str__(self):
        if self.p == None:
            return 'Binomial({})'.format(self.probabilities)
        else:
            return 'Binomial([1-{} {}])'.format(self.p, self.p)

    def __repr__(self):
        return self.__str__()

    def __add__(self, pmf): 
        if isinstance(pmf, Binomial):
            return self.get_concrete_probabilities() + pmf.get_concrete_probabilities() 
        else:
            return self.get_concrete_probabilities() + pmf

    def __mul__(self, pmf): 
        if isinstance(pmf, Binomial):
            return self.get_concrete_probabilities() * pmf.get_concrete_probabilities() 
        else:
            return self.get_concrete_probabilities() * pmf

    def __floordiv__(self, pmf): 
        if isinstance(pmf, Binomial):
            return self.get_concrete_probabilities() / pmf.get_concrete_probabilities() 
        else:
            return self.get_concrete_probabilities() / pmf

    def __truediv__(self, pmf):
        return self.__floordiv__(pmf) 