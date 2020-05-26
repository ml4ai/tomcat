from ..distribution import Distribution
from scipy.stats import norm
from base.node import Node

class Gaussian(Distribution):

    """
    Class that represents a Gaussian distribution 
    """

    def __init__(self, mean, variance, transformation_mean=lambda x: x, transformation_variance=lambda x: x):
        self.mean = mean
        self.variance = variance     
        self.transformation_mean = transformation_mean
        self.transformation_variance = transformation_variance    

    def sample(self):
        mean, variance = self.get_concrete_parameters()
        normal = norm(mean, variance)             
        return normal.rvs()

    def get_concrete_parameters(self):
        """
        Retrieves the parameters. 
        """
        
        if isinstance(self.mean, Node):
            mean = self.transformation_mean(self.mean.assignment)
        else:
            mean = self.transformation_mean(self.mean)

        if isinstance(self.variance, Node):
            variance = self.transformation_variance(self.variance.assignment)
        else:
            variance = self.transformation_variance(self.variance)

        # print('{};{}'.format(mean, variance))
        return mean, variance

    def get_probability(self, states):
        """
        Retrieves the joint density of a given set of states
        """
        mean, variance = self.get_concrete_parameters()
        normal = norm(mean, variance)          
        return normal.pdf(states)

    def replace_parameter_node(self, node):
        """
        Replaces a parameter node by another of equivalent metadata
        """
        if isinstance(self.mean, Node) and self.mean.metadata == node.metadata:
            self.mean = node

        if isinstance(self.variance, Node) and self.variance.metadata == node.metadata:
            self.variance = node

    def __str__(self):
        return 'Gaussian({};{})'.format(self.mean, self.variance)

    def __repr__(self):
        return self.__str__()
