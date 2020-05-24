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
                self.pgm.nodes[node]['data'].assignment = assignment
            
            for t in range(self.pgm.time_slices):            
                nodes_in_time_slice = [node for node in nx.topological_sort(self.pgm) if node[1] == t]
                
                for node in nodes_in_time_slice:
                    if node in observations.keys():                        
                        assignment = observations[node] 

                    else:
                        parent_nodes = [parent_node for parent_node in self.pgm.predecessors(node)]           
                        parent_labels = [parent_node[0] for parent_node in parent_nodes]
                        cpd = [cpd for cpd in self.pgm.metadata.cpds 
                                if cpd.node.label == node[0] 
                                and set([parent_node.label for parent_node in cpd.parent_nodes]) == set(parent_labels)][0]
                        
                        parent_assignments = {parent_node[0]: self.pgm.nodes(data=True)[parent_node]['data'].assignment 
                                               for parent_node in parent_nodes}
                        distribution = cpd.get_distribution(observations=parent_assignments)
                        
                        assignment = distribution.sample()
                        self.pgm.nodes(data=True)[node]['data'].assignment = assignment
                    
                    try:
                        sample[node] = (assignment, self.pgm.nodes(data=True)[node]['data'].metadata.state_names.get(assignment, assignment))
                    except:
                        sample[node] = (assignment, assignment)

            samples.append(sample)

        return samples