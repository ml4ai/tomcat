from .ancestral_sampling import AncestralSampling
import copy
import networkx as nx
import numpy as np
from distribution.discrete.multinomial import Multinomial

class GibbsSampling:
    

    def __init__(self, pgm):
        self.pgm = copy.deepcopy(pgm)


    def sample(self, number_of_samples=1, burn_in_periods=100, observations={}):
        samples = []

        free_nodes = [node for node in self.pgm.nodes() if node not in observations.keys()]

        sample = AncestralSampling(self.pgm).sample(observations=observations)[0]
        # print(sample)
        for node, state in sample.items():
            self.pgm.nodes[node]['data'].assignment = state[0]

        for i in range(number_of_samples + burn_in_periods):
            for node in free_nodes:
                print('\nNode: {}'.format(node))

                evidence = {evidence_node[0]:self.pgm.nodes[evidence_node]['data'].assignment 
                            for evidence_node in self.pgm.nodes() if evidence_node != node}
                posterior = self.get_posterior(node, evidence)                

                assignment = posterior.sample()
                print('Node {} sampled as {}'.format(node, assignment))            
                self.pgm.nodes[node]['data'].assignment = assignment

                try:
                    sample[node] = (assignment, self.pgm.nodes(data=True)[node]['data'].metadata.state_names.get(assignment, assignment))
                except:
                    sample[node] = (assignment, assignment)

            if i >= burn_in_periods:
                samples.append(sample.copy())    

        return samples

    def get_posterior(self, node, evidence):
        print('Evidence: {}'.format(evidence))
        parent_nodes = [parent_node for parent_node in self.pgm.predecessors(node)]
        child_nodes = [child_node for child_node in self.pgm.successors(node)]

        node_given_parents_cpds = [cpd for cpd in self.pgm.metadata.cpds 
                                   if cpd.node.label == node[0]
                                   and set([parent_node.label for parent_node in cpd.parent_nodes]) 
                                   == set([parent_node[0] for parent_node in parent_nodes])]
        child_given_node_cpds = [cpd for cpd in self.pgm.metadata.cpds if node[0] 
                                 in [parent_node.label for parent_node in cpd.parent_nodes]]

        print('CPD(Node | Parents): {}'.format(node_given_parents_cpds))
        print('CPD(Child | Node): {}'.format(child_given_node_cpds))

        distributions_for_node_given_parents = [cpd.get_distribution(evidence) 
                                                for cpd in node_given_parents_cpds]
        
        print('D(Node | Parents): {}'.format(distributions_for_node_given_parents))
        posterior = distributions_for_node_given_parents[0].get_probabilities()
        print('Posterior 1: {}'.format(posterior))

        
        for cpd in child_given_node_cpds:
            print('D(Child | Node): {}'.format(cpd.get_distribution(evidence)))     
            posterior *= np.array([distribution.get_probability(evidence[cpd.node.label]) for distribution in cpd.get_distribution(evidence)])

        print('Posterior 2: {}'.format(posterior))
        z = np.sum(posterior)
        print('z: {}'.format(z))

        posterior = Multinomial(posterior/z)
        print('Final posterior: {}'.format(posterior))

        return posterior

