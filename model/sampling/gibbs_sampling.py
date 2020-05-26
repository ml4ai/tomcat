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

        sample = AncestralSampling(self.pgm).sample(observations=observations)[0]
        # print('Sample: {}'.format(sample))
        for node, state in sample.items():
            self.pgm.nodes(data='data')[node].assignment = state[0]

        free_nodes = [data['data'] for node_id, data in self.pgm.nodes(data=True) if node_id not in observations.keys()]
        for i in range(number_of_samples + burn_in_periods):
            for node in free_nodes:
                # print('\nNode: {}'.format(node))
                # print('\nCurrent Assignment: {}'.format(node.assignment))

                evidence = {evidence_node.metadata.label:evidence_node.assignment 
                            for _, evidence_node in self.pgm.nodes(data='data') if evidence_node != node}
                posterior = self.get_posterior(node, evidence)                

                assignment = posterior.sample()
                # print('Node {} sampled as {}'.format(node, assignment))            
                node.assignment = assignment

                try:
                    sample[node.get_id()] = (assignment, node.metadata.state_names.get(assignment, assignment))
                except:
                    sample[node.get_id()] = (assignment, assignment)

            if i >= burn_in_periods:
                samples.append(sample.copy())    

        return samples

    def get_posterior(self, node, evidence):
        # print('Evidence: {}'.format(evidence))
        
        # Change this when working with continuous in higher lebels of the PGM
        posterior = node.cpd.get_distribution(evidence).get_concrete_probabilities()
        # print('Posterior 1: {}'.format(posterior))

        child_given_node_cpds = [self.pgm.nodes(data='data')[child_node].cpd for child_node in self.pgm.successors(node.get_id())]
        # print('CPD(Child | Node): {}'.format(child_given_node_cpds))        
        for cpd in child_given_node_cpds:
            # print('D(Child | Node): {}'.format(cpd.get_distribution(evidence)))     
            posterior *= np.array([distribution.get_probability(evidence[cpd.node.label]) for distribution in cpd.get_distribution(evidence)])

        # print('Posterior 2: {}'.format(posterior))
        z = np.sum(posterior)
        # print('z: {}'.format(z))

        posterior = Multinomial(posterior[:-1]/z)
        # print('Final posterior: {}'.format(posterior))

        return posterior

