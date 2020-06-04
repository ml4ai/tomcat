from .ancestral_sampling import AncestralSampling
import copy
import pandas as pd
import numpy as np

class GibbsSampling:

    def __init__(self, pgm):
        self.pgm = copy.deepcopy(pgm)

    def sample(self, number_of_samples=1, burn_in_periods=100, observations=pd.Series([], dtype='object')):
        samples = []

        # Initial sample
        sample = AncestralSampling(self.pgm).sample(observations=observations)

        self.assign_values_to_nodes(sample)

        # Nodes we have to iterate over in the gibbs sampling, that is, the ones not observed in data
        latent_nodes = [node for node in self.pgm.get_nodes() if node.get_id() not in observations.columns]

        for i in range(number_of_samples + burn_in_periods):
            for node in latent_nodes:
                # print('\nNode: {}'.format(node))
                # #print('\nCurrent Assignment: {}'.format(node.assignment))
                posterior = self.get_posterior(node, pd.DataFrame(sample))
                assignment = posterior.sample()
                node.assignment = assignment
                sample[node.get_id()] = assignment


            if i >= burn_in_periods:
                print('Sample {}'.format(i - burn_in_periods + 1))
                samples.append(sample.copy())
            else:
                print('Burn-in Sample {}'.format(i + 1))

        return pd.DataFrame(samples)

    def assign_values_to_nodes(self, assignments):
        for node_id, assignment in assignments.items():
            node = self.pgm.get_node(node_id)
            node.assignment = assignment

    def get_posterior(self, main_node, data_and_samples):
        posterior = None
        # todo - remove parameters from the set of parents
        parents_assignments = self.get_parents_assignments_and_counts(main_node, data_and_samples=data_and_samples)

        for parents_assignment in parents_assignments:
            # Since it's guaranteed we are passing the whole set of parents, this will return just one distribution
            # and we can access it directly from the first index of the array
            # P(main_node | parents)
            main_node_distribution = \
                main_node.cpd.get_distribution(self.remove_time_slice_indicator(parents_assignment['assignment']))[0]
            # The parent_assignment_frequency here will only be different than 1 if we are dealing
            # with a constant node outside a plate that is not a parameter node and is a child of nodes
            # inside a plate. This speeds up the process by not looping over each data point
            # but instead using the frequencies
            if posterior == None:
                posterior = main_node_distribution.power(parents_assignment['frequency'])
            else:
                posterior = posterior.mult(main_node_distribution.power(parents_assignment['frequency']))

            child_nodes = [self.pgm.nodes(data='data')[child_node] for child_node in
                           self.pgm.successors(main_node.get_id())]

            for child_node in child_nodes:
                # Get assignments for the parents of the child node, including the child assignment but excluding
                # main_node which happens to be one of the parents of the child node
                child_parents_assignments = self.get_parents_assignments_and_counts(child_node,
                                                                                    data_and_samples=data_and_samples,
                                                                                    exclusions=[main_node.get_id()])

                for child_parents_assignment in child_parents_assignments:
                    child_assignment_frequency = self.get_node_frequency(child_node,
                                                                         child_parents_assignment['assignment'],
                                                                         data_and_samples)

                    # Since one the parents of the child node is not included in this list, a set of distributions
                    # will be returned. One for each possible value of the excluded parent node (main_node)
                    child_distributions = child_node.cpd.get_distribution(
                        self.remove_time_slice_indicator(child_parents_assignment['assignment']))

                    probabilities_from_child_distribution = []
                    for child_distribution in child_distributions:
                        if main_node.metadata.parameter:
                            if child_distribution.depends_on(main_node):
                                # this code is executed only once per child because if we are dealing with a parameter,
                                # there will be just one element in child_distributions
                                posterior = posterior.conjugate(child_distribution, child_assignment_frequency)
                            else:
                                # This can only happen if we are sampling a parameter that is not the one assigned for
                                # the current assignment of parents.
                                pass
                        else:
                            # The child_assignment_frequency here will only be different than 1 if we are dealing
                            # with a constant node outside a plate that is not a parameter node and is linked to
                            # nodes inside a plate. This speeds up the process by not looping over each data point
                            # but instead using the frequencies. (E.g. node Q in the ToMCAT model)
                            probabilities_from_child_distribution.append(
                                child_distribution.get_probability(child_assignment_frequency))

                    posterior = posterior.mult(probabilities_from_child_distribution)

    def get_parents_assignments_and_counts(self, node, data_and_samples=pd.DataFrame(), exclusions=[]):
        parent_ids = self.pgm.get_parent_nodes_id_of(node)

        # Create a slice of the data_and_samples just with the parents
        if parent_ids != []:
            data_and_samples_from_parents = data_and_samples.loc[:, parent_ids]
        else:
            data_and_samples_from_parents = pd.DataFrame()

        # Exclude parent nodes if requested
        data_and_samples_from_parents.drop(exclusions, axis=1, inplace=True)

        # Remove duplicates and creates a new column with the frequency of such duplicates
        data_and_samples_from_parents = data_and_samples_from_parents.groupby(
            data_and_samples_from_parents.columns.values.tolist()).size().reset_index()

        # For each row, create a dictionary containing the assignments and frequency as separate keys
        assignments_and_counts = []
        frequency_column = data_and_samples_from_parents.columns[-1]
        for _, series in data_and_samples_from_parents.iterrows():
            assignments_and_counts.append(
                {'assignment': series.drop(frequency_column), 'frequency': series[frequency_column]})

        if assignments_and_counts == []:
            assignments_and_counts = [{'assignment': pd.Series([], dtype='object'), 'frequency': 1}]

        return assignments_and_counts

    def remove_time_slice_indicator(self, assignment):
        if assignment.empty:
            return assignment
        else:
            assignment.index = pd.MultiIndex.from_tuples(assignment.index)
            return assignment.droplevel(1)

    def get_node_frequency(self, node, query, data_and_samples):
        data_and_samples_given_query = data_and_samples.loc[np.all(data_and_samples[list(query)] == pd.Series(query), axis=1)]
        frequencies = data_and_samples_given_query[node.get_id()].value_counts()

        return frequencies