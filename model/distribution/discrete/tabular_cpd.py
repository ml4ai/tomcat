from base.node import Node
import numpy as np

class TabularCPD:

    """
    Class that represents discrete conditional probabilities of a node given its parent nodes. 
    In case a node has no parent node, this class represents the priors of such a node.

    The values inputed must be a matrix of dimension 
    (cardinality of the main node)x(product of the cardinality of its parent nodes)

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
    """
    
    def __init__(self, node, parent_nodes, values):
        """
        Constructor

        Parameters
        ----------
        node: main node
        parent_nodes: parent nodes of the main node for which the conditional probability is being defined
        values: matrix containing the probabilities for each combintion os states between the main node and its parents
        """
        if not isinstance(node, Node):
            raise TypeError('The main node has to be an instance of the class Node')

        if not all(isinstance(parent_node, Node) for parent_node in parent_nodes):
            raise TypeError('All the parent nodes have to be instances of the class Node')    

        self.node = node
        self.parent_nodes = parent_nodes
        self.set_values(values)
        
    def set_values(self, values):
        """
        Converts the matrix of probabilities to a ndimensional of the form 
        dim_parent_1 x dim_parent_2 x ... x dim_parent_k x dim_main_node

        This will make indexing easier.
        """

        parents_cardinalities = [parent_node.cardinality for parent_node in self.parent_nodes]
        cardinalities = tuple(parents_cardinalities + [self.node.cardinality])
        self.values = np.reshape(values.T, cardinalities)
    
    # def get_parent_node_labels(self):
    #     return [parent_node.label for parent_node in self.parent_nodes]        
    
    def get_distribution(self, observations):        
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