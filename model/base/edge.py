from .node import Node

class Edge:

    """
    Class that represents a directed edge between two nodes. 
    """
    
    def __init__(self, node_from, node_to, forward_in_time=False):
        """
        Constructor

        Parameters
        ----------
        node_from: origin node
        node_to: target node
        forward_in_time: nodes are linked forward in time (t -> t+1) in the extended model. It 
                         has no effect if one of the nodes is extendable              
        """

        if not isinstance(node_from, Node) or not isinstance(node_to, Node):
            raise TypeError('The nodes have to be instances of the class Node')
        
        self.node_from = node_from
        self.node_to = node_to
        self.forward_in_time = forward_in_time
    
    def has_node_in(self, nodes):
        return self.node_from in nodes or self.node_to in nodes
    
    def has_both_nodes_in(self, nodes):
        return self.node_from in nodes and self.node_to in nodes
    
    def __str__(self):
        if self.forward_in_time:
            return '{} -(t)-> {}'.format(self.node_from, self.node_to)
        else:
            return '{} --> {}'.format(self.node_from, self.node_to)
    
    def __repr__(self):
        return str(self)
    
    def __hash__(self):
        return hash(repr(self))
    
    def __eq__(self, other):
        return self.__class__ == other.__class__ and repr(self) == repr(other)