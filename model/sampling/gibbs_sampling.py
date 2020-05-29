from .ancestral_sampling import AncestralSampling
import copy
import pandas as pd

class GibbsSampling:

    def __init__(self, pgm):
        self.pgm = copy.deepcopy(pgm)

    def sample(self, number_of_samples=1, burn_in_periods=100, observations=pd.DataFrame()):
        samples = []
        labeled_samples = []

        if observations.empty:
            single_observation = pd.Series([], dtype='object')
            # The following transformation is not needed but it makes the series indexes prettier
            single_observation.index = pd.MultiIndex.from_tuples(single_observation.index)
        else:
            single_observation = observations.iloc[0]

        sample, labeled_sample = AncestralSampling(self.pgm).sample(observations=single_observation)
        sample = sample.iloc[0].to_dict()
        labeled_sample = labeled_sample.iloc[0].to_dict()

        for node_id, assignment in sample.items():
            self.pgm.nodes(data='data')[node_id].assignment = assignment

        latent_nodes = [node for _, node in self.pgm.nodes(data='data') if node.get_id() not in observations.columns]

        for i in range(number_of_samples + burn_in_periods):
            for node in latent_nodes:
                #print('\nNode: {}'.format(node))
                # #print('\nCurrent Assignment: {}'.format(node.assignment))

                parent_assignments = pd.Series({parent_id[0]:sample[parent_id] for parent_id in self.pgm.predecessors(node.get_id())})
                children_assignments = {}
                for child_id in self.pgm.successors(node.get_id()):
                    children_assignments.update({child_id[0]:sample[child_id]})
                    children_assignments.update({parent_id[0]:sample[parent_id] for parent_id in self.pgm.predecessors(child_id) if parent_id != node.get_id()})
                children_assignments = pd.Series(children_assignments)

                posterior = self.get_posterior(node, parent_assignments, children_assignments, observations)

                assignment = posterior.sample()
                # #print('Node {} sampled as {}'.format(node, assignment))            
                node.assignment = assignment

                sample[node.get_id()] = assignment
                if node.metadata.state_names != {} and assignment in node.metadata.state_names.keys():
                    labeled_sample[node.get_id()] = node.metadata.state_names[assignment]
                else:
                    labeled_sample[node.get_id()] = assignment

            if i >= burn_in_periods:
                print('Sample {}'.format(i-burn_in_periods+1))
                samples.append(sample.copy())
                labeled_samples.append(labeled_sample.copy())

        return pd.DataFrame(samples), pd.DataFrame(labeled_samples)

    def get_posterior(self, node, parent_assignments, children_assignments, observations):
        posterior = node.cpd.get_distribution(parent_assignments)[0]

        child_given_node_cpds = [self.pgm.nodes(data='data')[child_node].cpd for child_node in self.pgm.successors(node.get_id())]
        #print('CPD(Child | Node): {}'.format(child_given_node_cpds))
        for cpd in child_given_node_cpds:
            #print('D(Child | Node): {}'.format(cpd.get_distribution(children_assignments)))
            for i, distribution in enumerate(cpd.get_distribution(children_assignments)):
                # Each child node has a distribution for a state of the parent node or the child node has has a distribution which
                # the parameters are generated from the parent node
                try:
                    # Select in the dataframe all the entries with node and cpd.node given that the other nodes have values
                    # according to the observations got so far
                    #print('Child State: {}'.format(children_assignments[cpd.node.label]))
                    #print('Node State: {}'.format(i))
                    posterior = posterior.mult(distribution, children_assignments[cpd.node.label], i)
                except TypeError:
                    # The instantiation of the parent does not depend on the parameter
                    # Eg. node = theta_g3 but i = 0 and d = 0 which yields theta_g0 in g
                    pass

        return posterior