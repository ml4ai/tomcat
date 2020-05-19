import networkx as nx
from base.node import Node
from base.edge import Edge
from .pgm_metadata import PGMMetadata

class PGM(nx.DiGraph):

    """
    Class that represents a PGM unrolled in time slices. 

    The pgm will be unrolled in several time slices and represented internally by a graph. Here the edges that
    are forward in time will connect nodes from different time slices and nodes are added to the graph according
    to their defined first-occurrence time slice and repeatability. In the graph, a node is uniquely identified
    as a tuple formed by its label and the time slice they are in. 
    """
    
    def __init__(self, metadata, time_slices=1):
        """
        Constructor

        Parameters
        ----------
        metadata: generic definition of the pgm
        time_slices: number of time slices to unroll
        """

        super().__init__()
        self.metadata = metadata
        self.time_slices = time_slices
        self.build_graph()
            
    def build_graph(self):
        nodes_in_previous_time_slice = []
        
        for t in range(self.time_slices):
            nodes_in_time_slice = [node for node in self.metadata.nodes 
                                    if (node.time_slice <= t and node.repeatable) 
                                    or ( not node.repeatable and node.time_slice == t)]

            edges_in_time_slice = [edge for edge in self.metadata.edges 
                                    if not edge.forward_in_time 
                                    and edge.has_both_nodes_in(nodes_in_time_slice)]            

            edges_between_time_slices = [edge for edge in self.metadata.edges 
                                          if edge.forward_in_time 
                                          and edge.has_node_in(nodes_in_previous_time_slice)]

            extendable_nodes = [node for node in self.metadata.nodes if (node.time_slice <= t and node.extendable)]                       
            edges_for_extended_nodes = [edge for edge in self.metadata.edges if edge.has_node_in(extendable_nodes)]                                          
            
            nodes_ids = [((node.label, t), {'cardinality':node.cardinality, 'state_names':node.state_names}) 
                          for node in nodes_in_time_slice]

            edges_in_time_slice_ids = [[(edge.node_from.label, t), (edge.node_to.label, t)] 
                                        for edge in edges_in_time_slice]

            edges_between_time_slice_ids = [[(edge.node_from.label, t-1), (edge.node_to.label, t)] 
                                             for edge in edges_between_time_slices]
            
            edges_for_extended_nodes_ids = [[(edge.node_from.label, edge.node_from.time_slice), (edge.node_to.label, t)]
                                             if edge.node_from.extendable 
                                             else 
                                             [(edge.node_from.label, t), (edge.node_to.label, edge.node_to.time_slice)]
                                             for edge in edges_for_extended_nodes]           

            edges_ids = edges_in_time_slice_ids + edges_between_time_slice_ids + edges_for_extended_nodes_ids
            
            self.add_nodes_from(nodes_ids)
            self.add_edges_from(edges_ids)
            
            nodes_in_previous_time_slice = nodes_in_time_slice  