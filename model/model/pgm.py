import networkx as nx
from base.node import Node
from .pgm_metadata import PGMMetadata
import copy


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
        self.assign_cpds()

    def build_graph(self):
        nodes_in_previous_time_slice = []

        for t in range(self.time_slices):
            nodes_in_time_slice = [node for node in self.metadata.nodes
                                   if (node.first_time_slice <= t and node.repeatable)
                                   or (not node.repeatable and node.first_time_slice == t)]

            graph_nodes = [((node_metadata.label, t), {'data': Node(node_metadata, t)}) for node_metadata in
                           nodes_in_time_slice]

            edges_in_time_slice = [edge for edge in self.metadata.edges
                                   if not edge.forward_in_time
                                   and edge.has_both_nodes_in(nodes_in_time_slice)]

            edges_between_time_slices = [edge for edge in self.metadata.edges
                                         if edge.forward_in_time
                                         and edge.has_node_in(nodes_in_previous_time_slice)]

            constant_nodes = [node for node in self.metadata.nodes
                              if (node.first_time_slice <= t and node.constant)]

            edges_for_constant_nodes = [edge for edge in self.metadata.edges
                                        if edge.has_node_in(constant_nodes)
                                        and edge.has_node_in(graph_nodes)]

            graph_edges_in_time_slice = [[(edge.node_from.label, t), (edge.node_to.label, t)]
                                         for edge in edges_in_time_slice]

            graph_edges_between_time_slice = [[(edge.node_from.label, t - 1), (edge.node_to.label, t)]
                                              for edge in edges_between_time_slices]

            graph_edges_for_constant_nodes = [
                [(edge.node_from.label, edge.node_from.first_time_slice), (edge.node_to.label, t)]
                if edge.node_from.extendable
                else
                [(edge.node_from.label, t), (edge.node_to.label, edge.node_to.time_slice)]
                for edge in edges_for_constant_nodes]

            graph_edges = graph_edges_in_time_slice + graph_edges_between_time_slice + graph_edges_for_constant_nodes

            graph_nodes.sort()
            self.add_nodes_from(graph_nodes)
            self.add_edges_from(graph_edges)

            nodes_in_previous_time_slice = nodes_in_time_slice

    def assign_cpds(self):
        for node_id, node in self.nodes(data='data'):
            parent_nodes = [self.nodes(data='data')[parent_node_id]
                            for parent_node_id in self.predecessors(node_id)]
            parents_metadata = [parent_node.metadata for parent_node in parent_nodes]
            cpds_for_node = [cpd for cpd in self.metadata.cpds
                             if cpd.node == node.metadata
                             and set(cpd.parent_nodes) == set(parents_metadata)]
            node.cpd = copy.deepcopy(cpds_for_node[0])
            node.cpd.replace_parameter_node(parent_nodes)

    def get_constant_nodes(self):
        return [node for node in self.get_nodes() if node.metadata.constant == True]

    def get_parameter_nodes(self):
        return [node for node in self.get_nodes() if node.metadata.parameter == True]

    def get_parameter_nodes_id(self):
        return [node.get_id() for node in self.get_nodes() if node.metadata.parameter == True]

    def get_nodes(self):
        return [node for _, node in self.nodes(data='data')]

    def get_node(self, node_id):
        return self.nodes(data='data')[node_id]

    def get_parent_nodes_of(self, node, include_parameter_nodes=False):
        if include_parameter_nodes:
            return [self.get_node(parent_node_id) for parent_node_id in self.predecessors(node.get_id())]
        else:
            return [self.get_node(parent_node_id) for parent_node_id in self.predecessors(node.get_id()) if
                    not self.get_node(parent_node_id).metadata.parameter]

    def get_parent_nodes_id_of(self, node, include_parameter_nodes=False):
        if include_parameter_nodes:
            return [parent_node_id for parent_node_id in self.predecessors(node.get_id())]
        else:
            return [parent_node_id for parent_node_id in self.predecessors(node.get_id()) if
                    not self.get_node(parent_node_id).metadata.parameter]
