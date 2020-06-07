import networkx as nx
import copy
import pandas as pd
from tqdm import tqdm

class AncestralSampling:   

    """
    Class that generate data using ancestral sampling technique in a PGM
    """
    def __init__(self, pgm):
        self.pgm = copy.deepcopy(pgm)

    def sample(self, number_of_samples=1, observations=pd.Series([], dtype='object')):
        samples = []

        for _ in tqdm(range(number_of_samples), desc="Samples", disable=(number_of_samples==1)):
            sample = {}
            labeled_sample = {}

            for node_id, assignment in observations.items():
                self.pgm.nodes(data='data')[node_id].assignment = assignment
            
            for t in range(self.pgm.time_slices):            
                nodes_in_time_slice = [self.pgm.nodes(data='data')[node_id] for node_id in nx.topological_sort(self.pgm) if node_id[1] == t]
                
                for node in nodes_in_time_slice:
                    if node.get_id() in observations.index:
                        assignment = observations[node.get_id()]

                    else:
                        parent_assignments = pd.Series({parent_id[0]:sample[parent_id] for parent_id in self.pgm.predecessors(node.get_id())})
                        distribution = node.cpd.get_distribution(observations=parent_assignments)[0]
                        assignment = distribution.sample()
                        node.assignment = assignment
                    
                    sample[node.get_id()] = assignment

            samples.append(sample.copy())

        return pd.DataFrame(samples)