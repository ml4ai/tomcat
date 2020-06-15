import networkx as nx
from base.node import Node
from .pgm_metadata import PGMMetadata
import copy
from tqdm import tqdm


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
        print('1')
        self.build_graph()
        print('2')
        self.assign_cpds()

    def build_graph(self):
        nodes_in_previous_time_slice = []

        for t in range(self.time_slices):
            nodes_in_time_slice = []
            graph_nodes = []
            constant_nodes = []

            for node_metadata in tqdm(self.metadata.nodes, desc='Creating Nodes (t = {})'.format(t)):
                if (node_metadata.first_time_slice <= t and node_metadata.repeatable) or (
                        not node_metadata.repeatable and node_metadata.first_time_slice == t):
                    nodes_in_time_slice.append(node_metadata)
                    graph_node = ((node_metadata.label, t), {'data': Node(node_metadata, t)})
                    graph_nodes.append(graph_node)
                elif node_metadata.first_time_slice <= t and node_metadata.constant and not node_metadata.prior:
                    constant_nodes.append(node_metadata)

            # nodes_in_time_slice = [node for node in self.metadata.nodes
            #                        if (node.first_time_slice <= t and node.repeatable)
            #                        or (not node.repeatable and node.first_time_slice == t)]

            # graph_nodes = [((node_metadata.label, t), {'data': Node(node_metadata, t)}) for node_metadata in
            #                nodes_in_time_slice]

            graph_edges = []
            for edge in tqdm(self.metadata.edges, desc='Creating Edges (t = {})'.format(t)):
                graph_edge = None
                if not edge.forward_in_time and edge.has_both_nodes_in(nodes_in_time_slice):
                    # Edges in the same time slice
                    graph_edge = [(edge.node_from.label, t), (edge.node_to.label, t)]

                elif edge.forward_in_time and edge.has_node_in(nodes_in_previous_time_slice):
                    # Edges between time slices
                    graph_edge = [(edge.node_from.label, t - 1), (edge.node_to.label, t)]

                elif edge.has_node_in(constant_nodes) and edge.has_node_in(nodes_in_time_slice):
                    # Edges from constant nodes
                    if edge.node_from.constant:
                        graph_edge = [(edge.node_from.label, edge.node_from.first_time_slice), (edge.node_to.label, t)]
                    else:
                        graph_edge = [(edge.node_from.label, t), (edge.node_to.label, edge.node_to.first_time_slice)]

                if graph_edge != None:
                    graph_edges.append(graph_edge)

            # edges_in_time_slice = [edge for edge in self.metadata.edges
            #                        if not edge.forward_in_time
            #                        and edge.has_both_nodes_in(nodes_in_time_slice)]
            #
            # edges_between_time_slices = [edge for edge in self.metadata.edges
            #                              if edge.forward_in_time
            #                              and edge.has_node_in(nodes_in_previous_time_slice)]

            # constant_nodes = [node for node in self.metadata.nodes
            #                   if (node.first_time_slice <= t and node.constant and not node.prior)]

            # edges_for_constant_nodes = [edge for edge in self.metadata.edges
            #                             if edge.has_node_in(constant_nodes)
            #                             and edge.has_node_in(nodes_in_time_slice)]

            # graph_edges_in_time_slice = [[(edge.node_from.label, t), (edge.node_to.label, t)]
            #                              for edge in edges_in_time_slice]

            # graph_edges_between_time_slice = [[(edge.node_from.label, t - 1), (edge.node_to.label, t)]
            #                                   for edge in edges_between_time_slices]

            # graph_edges_for_constant_nodes = [
            #     [(edge.node_from.label, edge.node_from.first_time_slice), (edge.node_to.label, t)]
            #     if edge.node_from.constant
            #     else
            #     [(edge.node_from.label, t), (edge.node_to.label, edge.node_to.time_slice)]
            #     for edge in edges_for_constant_nodes]
            #
            # graph_edges = graph_edges_in_time_slice + graph_edges_between_time_slice + graph_edges_for_constant_nodes

            graph_nodes.sort()
            self.add_nodes_from(graph_nodes)
            self.add_edges_from(graph_edges)

            nodes_in_previous_time_slice = nodes_in_time_slice

    def assign_cpds(self):
        for node in self.get_nodes():
            parent_nodes = self.get_parent_nodes_of(node, include_parameter_nodes=True)
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

    def get_child_nodes_of(self, node, include_parameter_nodes=False):
        return [self.get_node(child_node_id) for child_node_id in self.successors(node.get_id())]

    def get_child_nodes_of(self, node, include_parameter_nodes=False):
        if include_parameter_nodes:
            return [self.get_node(child_node_id) for child_node_id in self.successors(node.get_id())]
        else:
            return [self.get_node(child_node_id) for child_node_id in self.successors(node.get_id()) if
                    not self.get_node(child_node_id).metadata.parameter]

    def get_child_nodes_id_of(self, node, include_parameter_nodes=False):
        if include_parameter_nodes:
            return [child_node_id for child_node_id in self.successors(node.get_id())]
        else:
            return [child_node_id for child_node_id in self.successors(node.get_id()) if
                    not self.get_node(child_node_id).metadata.parameter]

    def get_markov_blanket(self, node):
        markov_blanket = set()
        markov_blanket.update(set(self.get_parent_nodes_of(node)))
        for child_node in self.get_child_nodes_of(node):
            markov_blanket.add(child_node)
            markov_blanket.update(set(self.get_parent_nodes_of(child_node)))
        markov_blanket.remove(node)

        return markov_blanket

    def get_markov_blanket_ids(self, node):
        return self.get_ids_for(self.get_markov_blanket(node))

    def get_ids_for(self, nodes):
        return [node.get_id() for node in nodes]

    def fill_parameters(self, parameter_values):
        """
        This function replaces node assignments in the distributions from the CPDS by actual values
        """
        for node in self.get_nodes():
            if node.metadata.parameter:
                if node.metadata.label in parameter_values.index:
                    self.remove_node(node.get_id())
            else:
                for distribution in node.cpd.get_distributions():
                    distribution.fill_parameters(parameter_values)
                self.remove_parameter_nodes_from_cpd_for(node, parameter_values.index)

        self.metadata.fill_parameters(parameter_values)

    def remove_parameter_nodes_from_cpd_for(self, node, parameter_nodes_id):
        for parent_metadata in node.cpd.parent_nodes.copy():
            if parent_metadata.label in parameter_nodes_id:
                node.cpd.parent_nodes.remove(parent_metadata)

                # for cpd in self.metadata.cpds:
                #     for distribution in cpd.get_distributions():
                #         distribution.fill_parameters(parameter_values)

    # def remove_node(self, n):
    #     self.remove_node_from_metadata(self.get_node(n))
    #     super().remove_node(n)

    # def remove_node_from_metadata(self, node):
    #     # Remove node
    #     self.metadata.nodes.remove(node.metadata)
    #
    #     # Remove cpd for the node
    #     for cpd in list(self.metadata.cpds):
    #         if cpd.node == node.metadata:
    #             self.metadata.cpds.remove(cpd)
    #         else:
    #             if node.get_id() in cpd.parent_nodes:
    #                 cpd.parent_nodes.remove(node.get_id())
    #
    #     # Remove edges
    #     for edge in list(self.metadata.edges):
    #         if edge.node_from == node.metadata or edge.node_to == node.metadata:
    #             self.metadata.edges.remove(edge)
