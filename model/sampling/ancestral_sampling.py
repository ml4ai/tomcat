import networkx as nx
from model.pgm import PGM
import copy
import numpy as np

class AncestralSampling:   

    """
    Class that generate data using ancestral sampling technique in a PGM
    """
    
    def __init__(self, pgm):
        self.pgm = copy.deepcopy(pgm)
    
    def sample(self, number_of_samples=1, observations={}):
        samples = []
        
        for s in range(number_of_samples):
            sample = {}

            for node, assignment in observations.items():
                self.pgm.nodes(data='data')[node].assignment = assignment
            
            for t in range(self.pgm.time_slices):            
                nodes_in_time_slice = [self.pgm.nodes(data='data')[node_id] for node_id in nx.topological_sort(self.pgm) if node_id[1] == t]
                
                for node in nodes_in_time_slice:
                    if node.get_id() in observations.keys():                        
                        assignment = observations[node.get_id()] 

                    else:
                        parent_nodes = [self.pgm.nodes(data='data')[parent_node_id] for parent_node_id in self.pgm.predecessors(node.get_id())]                                                                         
                        parent_assignments = {parent_node.metadata.label: parent_node.assignment for parent_node in parent_nodes}
                        distribution = node.cpd.get_distribution(observations=parent_assignments)
                        assignment = distribution.sample()
                        node.assignment = assignment
                    
                    try:
                        sample[node.get_id()] = (assignment, node.metadata.state_names.get(assignment, assignment))
                    except:
                        sample[node.get_id()] = (assignment, assignment)

            samples.append(sample)

        return samples