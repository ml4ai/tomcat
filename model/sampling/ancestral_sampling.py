import networkx as nx
from model.pgm import PGM
import copy
import numpy as np

class AncestralSampling:   

    """
    Class that generate data using ancestral sampling technique in a PGM
    """
    
    def __init__(self, pgm):
        self.pgm = pgm
    
    def sample(self, number_of_samples=1, observations={}):
        model = copy.deepcopy(self.pgm)
        samples = []
        
        for s in range(number_of_samples):
            nx.set_node_attributes(model, -1, 'assigned_state')

            sample = {}

            for node, assignment in observations.items():
                model.nodes[node]['assigned_state'] = assignment
            
            for t in range(model.time_slices):            
                nodes_in_time_slice = [node for node in nx.topological_sort(model) if node[1] == t]
                
                for node in nodes_in_time_slice:
                    if node in observations.keys():                        
                        assignment = observations[node] 

                    else:
                        parent_nodes = [parent_node for parent_node in model.predecessors(node)]           
                        parent_labels = [parent_node[0] for parent_node in parent_nodes]
                        cpd = [cpd for cpd in model.metadata.cpds 
                                if cpd.node.label == node[0] 
                                and set([parent_node.label for parent_node in cpd.parent_nodes]) == set(parent_labels)][0]
                        
                        parent_assignments = {parent_node[0]: model.nodes(data=True)[parent_node]['assigned_state'] 
                                               for parent_node in parent_nodes}
                        distribution = cpd.get_distribution(observations=parent_assignments)
                        
                        assignment = distribution.sample()
                        model.nodes(data=True)[node]['assigned_state'] = assignment
                    
                    try:
                        sample[node] = (assignment, model.nodes(data=True)[node]['state_names'].get(assignment, assignment))
                    except:
                        sample[node] = (assignment, assignment)

            samples.append(sample)

        return samples