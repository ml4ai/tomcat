class PGMMetadata:
    
    """
    Class that represents a generic PGM. 
    """

    def __init__(self):
        self.nodes = set()
        self.edges = set()
        self.cpds = set()
    
    def add_node(self, node_metadata):
        self.nodes.add(node_metadata)
        
    def add_nodes_from(self, nodes_metadata):
        self.nodes.update(nodes_metadata)
        
    def add_edge(self, edge_metadata):
        self.edges.add(edge_metadata)
        
    def add_edges_from(self, edges_metadata):
        self.edges.update(edges_metadata)
        
    def add_cpds_from(self, cpds):
        self.cpds.update(cpds)

    def fill_parameters(self, parameter_values):
        """
        This function replaces node assignments in the distributions from the CPDS by actual values
        """
        for cpd in list(self.cpds):
            for distribution in cpd.get_distributions():
                distribution.fill_parameters(parameter_values)

        self.remove_parameter_nodes(parameter_values.index)

    def remove_parameter_nodes(self, parameter_nodes_id):
        for node in list(self.nodes):
            if node.parameter and node.label in parameter_nodes_id:
                self.nodes.remove(node)
                self.remove_node_from_cpds(node)
                self.remove_edges_with(node)

    def remove_node_from_cpds(self, node):
        for cpd in list(self.cpds):
            if cpd.node == node:
                self.cpds.remove(cpd)
            else:
                if node in cpd.parent_nodes:
                    cpd.parent_nodes.remove(node)

    def remove_edges_with(self, node):
        for edge in list(self.edges):
            if edge.node_from == node or edge.node_to == node:
                self.edges.remove(edge)