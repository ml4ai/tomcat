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
