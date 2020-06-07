from .ancestral_sampling import AncestralSampling
import copy
import pandas as pd
import numpy as np
from tqdm import tqdm

class GibbsSampling:

    def __init__(self, pgm):
        self.pgm = copy.deepcopy(pgm)

    def sample(self, number_of_samples=1, burn_in_periods=100, observations=pd.Series([], dtype='object')):
        samples = []

        # Initial sample
        sample = AncestralSampling(self.pgm).sample(observations=observations).iloc[0]

        self.assign_values_to_nodes(sample)

        # Nodes we have to iterate over in the gibbs sampling, that is, the ones not observed in data
        latent_nodes = [node for node in self.pgm.get_nodes() if node.get_id() not in observations.index]

        for i in tqdm(range(burn_in_periods), desc="Burn-in"):
            sample = self.get_single_sample_from_posterior(latent_nodes, sample)

        for i in tqdm(range(number_of_samples), desc="Samples"):
            sample = self.get_single_sample_from_posterior(latent_nodes, sample)
            samples.append(sample.copy())

        return pd.DataFrame(samples)

    def get_single_sample_from_posterior(self, latent_nodes, last_sample):
        for latent_node in latent_nodes:
            posterior = self.get_posterior(latent_node, pd.DataFrame(last_sample).transpose())
            assignment = posterior.sample()
            latent_node.assignment = assignment
            last_sample[latent_node.get_id()] = assignment

        return last_sample

    def assign_values_to_nodes(self, assignments):
        for node_id, assignment in assignments.items():
            node = self.pgm.get_node(node_id)
            node.assignment = assignment

    def get_posterior(self, latent_node, data_and_samples):
        posterior = None
        parents_assignments = self.get_parents_assignments_and_counts(latent_node, data_and_samples=data_and_samples)

        for parents_assignment in parents_assignments:
            # Since it's guaranteed we are passing the whole set of parents, this will return just one distribution
            # and we can access it directly from the first index of the array
            # P(latent_node | parents)
            main_node_distribution = \
                latent_node.cpd.get_distribution(self.remove_time_slice_indicator(parents_assignment['assignment']))[0]
            # The parent_assignment_frequency here will only be different than 1 if we are dealing
            # with a constant node outside a plate that is not a parameter node and is a child of nodes
            # inside a plate. This speeds up the process by not looping over each data point
            # but instead using the frequencies

            if latent_node.metadata.parameter:
                # todo - implement mult and power for linear distributions in case it's needed and get rid of
                #  this piece of code maintaining the contents of the else
                # Parents_assignments has only one occurrence since parameter nodes have no parents
                posterior = main_node_distribution
            else:
                if posterior == None:
                    posterior = main_node_distribution.power(parents_assignment['frequency'])
                else:
                    posterior = posterior.mult(main_node_distribution.power(parents_assignment['frequency']))

            child_nodes = self.pgm.get_child_nodes_of(latent_node)

            for child_node in child_nodes:
                # Get assignments for the parents of the child node, including the child assignment but excluding
                # latent_node which happens to be one of the parents of the child node
                exclusions = [] if latent_node.metadata.parameter else [latent_node.get_id()]
                child_parents_assignments = self.get_parents_assignments_and_counts(child_node,
                                                                                    data_and_samples=data_and_samples,
                                                                                    exclusions=exclusions)

                for child_parents_assignment in child_parents_assignments:
                    child_assignment_frequency = self.get_node_frequency(child_node,
                                                                         child_parents_assignment['assignment'],
                                                                         data_and_samples)

                    # Since one the parents of the child node is not included in this list, a set of distributions
                    # will be returned. One for each possible value of the excluded parent node (latent_node)
                    child_distributions = child_node.cpd.get_distribution(
                        self.remove_time_slice_indicator(child_parents_assignment['assignment']))

                    if latent_node.metadata.parameter:
                        # if we are dealing with a parameter, there will be just one element in child_distributions:
                        # the prior
                        child_distribution = child_distributions[0]
                        if child_distribution.depends_on(latent_node):
                            # As soon as we find the child parent's assignment that matches the latent node
                            # we can leave the loop
                            posterior = posterior.conjugate(child_distribution, child_assignment_frequency)
                            break
                        else:
                            pass
                            # This can only happen if we are sampling a parameter that is not the one assigned for
                            # the current assignment of parents.
                    else:
                        probabilities_from_child_distribution = []
                        for child_distribution in child_distributions:
                            # The child_assignment_frequency here will only be different than 1 if we are dealing
                            # with a constant node outside a plate that is not a parameter node and is linked to
                            # nodes inside a plate. This speeds up the process by not looping over each data point
                            # but instead using the frequencies. (E.g. node Q in the ToMCAT model)
                            probabilities_from_child_distribution.append(
                                child_distribution.get_probability(child_assignment_frequency, log_transform=True))

                        posterior = posterior.mult(probabilities_from_child_distribution, in_log_scale=True)

        return posterior

    def get_parents_assignments_and_counts(self, node, data_and_samples=pd.DataFrame(), exclusions=[]):
        parent_ids = self.pgm.get_parent_nodes_id_of(node) # Only parents that are not parameters

        # Create a slice of the data_and_samples just with the parents
        if parent_ids != []:
            data_and_samples_from_parents = data_and_samples.loc[:, parent_ids]
        else:
            data_and_samples_from_parents = pd.DataFrame()

        # Exclude parent nodes if requested
        data_and_samples_from_parents.drop(exclusions, axis=1, inplace=True)

        if data_and_samples_from_parents.empty:
            assignments_and_counts = [{'assignment': pd.Series([], dtype='object'), 'frequency': 1}]
        else:
            # Remove duplicates and creates a new column with the frequency of such duplicates
            data_and_samples_from_parents = data_and_samples_from_parents.groupby(
                data_and_samples_from_parents.columns.values.tolist()).size().reset_index()

            # For each row, create a dictionary containing the assignments and frequency as separate keys
            assignments_and_counts = []
            frequency_column = data_and_samples_from_parents.columns[-1]
            for _, series in data_and_samples_from_parents.iterrows():
                assignments_and_counts.append(
                    {'assignment': series.drop(frequency_column), 'frequency': series[frequency_column]})

        return assignments_and_counts

    def remove_time_slice_indicator(self, assignment):
        if assignment.empty:
            return assignment
        else:
            assignment.index = pd.MultiIndex.from_tuples(assignment.index)
            return assignment.droplevel(1)

    def get_node_frequency(self, node, query, data_and_samples):
        data_and_samples_given_query = data_and_samples.loc[np.all(data_and_samples[query.index] == pd.Series(query), axis=1)]
        frequencies = data_and_samples_given_query[node.get_id()].value_counts()

        return frequencies