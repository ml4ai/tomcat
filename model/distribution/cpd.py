from base.node_metadata import NodeMetadata
import numpy as np

class CPD:

    """
    Class that represents conditional probabilities of a node given its parent nodes. 
    In case a node has no parent node, this class represents the priors of such a node.

    The values inputed must be a list of distributions for each combination of parent states. 
    Therefore the dimension of such array is given by the product of the cardinality of its parent nodes.

    Example
    -------
    
    I = {intelligent, not intelligent}
    D = {easy, avg, hard}
    G = {gradeA, gradeB, gradeC}

    +-------+--------------------+------------------+
    |I      |    intelligent     | not intelligent  |
    +-------+-----+------+-------+------+----+------+
    |D      |easy | avg  | hard  | easy |avg |hard  |
    +-------+-----+------+-------+------+----+------+
    |gradeA |0.2  |  0.2 |  0.2  | 0.2  |0.2 | 0.2  |
    +-------+-----+------+-------+------+----+------+
    |gradeB |0.2  |  0.2 |  0.2  | 0.2  |0.2 | 0.2  |
    +-------+-----+------+-------+------+----+------+
    |gradeC |0.6  |  0.6 |  0.6  | 0.6  |0.6 | 0.6  |
    +-------+-----+------+-------+------+----+------+
    
    distribution = Multinomial([0.2, 0.2, 0.6])
    values = [distribution, distribution, distribution, distribution, distribution, distribution] 
    """
    
    def __init__(self, node, parent_nodes, values):
        """
        Constructor

        Parameters
        ----------
        node: main node metadata
        parent_nodes: metadada of the parent nodes of the main node for which the conditional probability is being defined
        values: list containing the distribution for each combination of states between the main node and its parents
        """
        if not isinstance(node, NodeMetadata):
            raise TypeError('The main node has to be an instance of the class Node')

        if not all(isinstance(parent_node, NodeMetadata) for parent_node in parent_nodes):
            raise TypeError('All the parent nodes have to be instances of the class Node')    

        self.node = node
        self.parent_nodes = parent_nodes
        self.set_values(values)
        
    def set_values(self, values):
        """
        Converts the matrix of probabilities to a ndimensional one of the form 
        dim_parent_1 x dim_parent_2 x ... x dim_parent_k

        This will make indexing easier.
        """

        parents_cardinalities = [parent_node.cardinality for parent_node in self.parent_nodes]
        cardinalities = tuple(parents_cardinalities)
        self.values = np.reshape(values, cardinalities)
    
    def get_distribution(self, observations={}):        
        """
        Retrieves the distribution of the main node given observations of some or all of its parents
        
        Parameters
        ----------
        observations: a dictionary where the keys are the parent node labels and the values are the 
                      index of the observed state

        Example
        -------

        {
            I:0
            D:2
        }

        It returns P(G | I=intelligent, D=hard)
        """

        indices = []
        
        for i, parent_node in enumerate(self.parent_nodes):
            if parent_node.label in observations.keys():
                indices.append(observations[parent_node.label])
            else:
                indices.append(slice(None))
        
        return self.values[tuple(indices)] 
    
    def __str__(self):
        parent_labels = sorted(self.parent_nodes, key=lambda p: repr(p))
        return '{}|{}'.format(repr(self.node), ','.join(map(str, parent_labels)))
        
    def __repr__(self):
        return str(self)
    
    def __hash__(self):
        return hash(repr(self))
    
    def __eq__(self, other):
        return self.__class__ == other.__class__ and repr(self) == repr(other)