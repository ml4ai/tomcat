import numpy as np
from ..distribution import Distribution
from base.node import Node

class Multinomial(Distribution):

    """
    Class that represents a Multinomial distribution
    """

    def __init__(self, probabilities, transformations=[]):
        self.probabilities = probabilities
        if transformations == []:
            self.transformations = [lambda x: x for _ in self.probabilities]
        else:
            self.transformations = transformations

    def sample(self):
        probabilities = self.get_concrete_probabilities()
        return np.random.choice(range(len(probabilities)), p=probabilities)

    def get_concrete_probabilities(self):
        """
        Retrieves the probabilities. 
        """
        
        probabilities = []

        for i, probability in enumerate(self.probabilities):
            if isinstance(probability, Node):
                probabilities.append(self.transformations[i](probability.assignment))
            else:
                probabilities.append(self.transformations[i](probability))

        probabilities = probabilities + [1 - np.sum(probabilities)]
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
        for i, probability in enumerate(self.probabilities):
            if isinstance(probability, Node) and probability.metadata == node.metadata:
                self.probabilities[i] = node

    def __str__(self):
        return 'Mult({})'.format(self.probabilities)

    def __repr__(self):
        return self.__str__()

    def __add__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.get_concrete_probabilities() + pmf.get_concrete_probabilities() 
        else:
            return self.get_concrete_probabilities() + pmf

    def __mul__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.get_concrete_probabilities() * pmf.get_concrete_probabilities() 
        else:
            return self.get_concrete_probabilities() * pmf

    def __floordiv__(self, pmf): 
        if isinstance(pmf, Multinomial):
            return self.get_concrete_probabilities() / pmf.get_concrete_probabilities() 
        else:
            return self.get_concrete_probabilities() / pmf

    def __truediv__(self, pmf):
        return self.__floordiv__(pmf)  