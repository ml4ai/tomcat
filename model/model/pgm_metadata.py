class PGMMetadata:
    
    """
    Class that represents a generic PGM. 
    """

    def __init__(self):
        self.nodes = set()
        self.edges = set()
        self.cpds = set()
    
    def add_node(self, node):
        self.nodes.add(node)
        
    def add_nodes_from(self, nodes):
        self.nodes.update(nodes)
        
    def add_edge(self, edge):
        self.edges.add(edge)
        
    def add_edges_from(self, edges):
        self.edges.update(edges)
        
    def add_cpds_from(self, cpds):
        self.cpds.update(cpds)
