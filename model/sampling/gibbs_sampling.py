from .ancestral_sampling import AncestralSampling
import copy
import pandas as pd
import numpy as np
import time


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

        # clock = time.clock()
        sample, labeled_sample = AncestralSampling(self.pgm).sample(observations=single_observation)
        # print('Elapsed clock time to run Ancestral sampling: {} seconds'.format(time.clock() - clock))

        # clock = time.clock()
        sample = sample.iloc[0].to_dict()
        labeled_sample = labeled_sample.iloc[0].to_dict()
        self.assign_values_to_nodes(sample)
        # print('Elapsed clock time to assign sample to nodes in the graph: {} seconds'.format(time.clock() - clock))

        latent_nodes = [node for _, node in self.pgm.nodes(data='data') if node.get_id() not in observations.columns]

        for i in range(number_of_samples + burn_in_periods):
            for node in latent_nodes:
                # print('\nNode: {}'.format(node))
                # #print('\nCurrent Assignment: {}'.format(node.assignment))

                # clock = time.clock()
                posterior = self.get_posterior(node, observations)
                # print('Elapsed clock time to compute the posterior: {} seconds'.format(
                #     time.clock() - clock))

                assignment = posterior.sample()
                # #print('Node {} sampled as {}'.format(node, assignment))            
                node.assignment = assignment

                sample[node.get_id()] = assignment
                if node.metadata.state_names != {} and assignment in node.metadata.state_names.keys():
                    labeled_sample[node.get_id()] = node.metadata.state_names[assignment]
                else:
                    labeled_sample[node.get_id()] = assignment

            if i >= burn_in_periods:
                print('Sample {}'.format(i - burn_in_periods + 1))
                samples.append(sample.copy())
                labeled_samples.append(labeled_sample.copy())
            else:
                print('Burn-in Sample {}'.format(i + 1))

        return pd.DataFrame(samples), pd.DataFrame(labeled_samples)

    def assign_values_to_nodes(self, assignments):
        for node_id, assignment in assignments.items():
            self.pgm.nodes(data='data')[node_id].assignment = assignment

    def get_posterior(self, main_node, observations):
        posterior = None
        parents_assignments = self.get_parents_assignments_and_counts(main_node.get_id(), observations=observations)

        for parents_assignment in parents_assignments:
            # Assign the values to the parent nodes. If the parent nodes assignment came through observations
            # we are going to loop over each one of the possible assignments, and we need to assign these
            # values to their actual nodes so the distributions that depend on them can see the correct value
            self.assign_values_to_nodes(parents_assignment['assignment'].to_dict())

            # Since it's guaranteed we are passing the whole set of parents, this will return just one distribution
            # and we can access it directly from the first index of the array
            main_node_distribution = \
                main_node.cpd.get_distribution(self.remove_time_slice_indicator(parents_assignment['assignment']))[0]
            if posterior == None:
                posterior = main_node_distribution.power(parents_assignment['frequency'])
            else:
                posterior = posterior.mult(main_node_distribution.power(parents_assignment['frequency']))

            child_nodes = [self.pgm.nodes(data='data')[child_node] for child_node in
                           self.pgm.successors(main_node.get_id())]

            for child_node in child_nodes:
                # Get assignments for the parents of the child node, including the child assignment but excluding
                # main_node which happens to be one of the parents of the child node
                child_parents_assignments = self.get_parents_assignments_and_counts(child_node.get_id(),
                                                                                    observations=observations,
                                                                                    exclusions=[main_node.get_id()])

                for child_parents_assignment in child_parents_assignments:
                    # todo - I need to get the frequency of child states let's say, g:0 3 times, g:1 5 times
                    #  given the parent assignment I am processing
                    child_assignment_frequency = self.get_node_frequency(child_node,
                                                                         child_parents_assignment['assignment'],
                                                                         observations)

                    # Since of the parents of the child node is not included in this list, a set of distributions
                    # will be returned. One for each possible value of the excluded parent node (main_node)
                    child_distributions = child_node.cpd.get_distribution(
                        self.remove_time_slice_indicator(child_parents_assignment['assignment']))

                    probabilities_from_child_distribution = []
                    for child_distribution in child_distributions:
                        if main_node.metadata.prior:
                            if child_distribution.depends_on(main_node):
                                # this code is executed only once because if we are dealing with a prior, there will be
                                # just one element in child_distributions
                                posterior = posterior.conjugate(child_distribution, child_assignment_frequency)
                        else:
                            probabilities_from_child_distribution.append(
                                child_distribution.get_probability(child_assignment_frequency))

                    posterior = posterior.mult(probabilities_from_child_distribution)

    def get_parents_assignments_and_counts(self, node_id, observations=pd.DataFrame(), exclusions=[]):
        parent_ids_observed = [parent_id for parent_id in self.pgm.predecessors(node_id) if
                               parent_id in observations.columns]

        # Create a slice of the observations just with the parents
        if parent_ids_observed != []:
            observations_from_parents = observations.loc[:, parent_ids_observed]
        else:
            observations_from_parents = pd.DataFrame()

        # For parent nodes that are not in the observations, create a new column for it in the dataframe and
        # broadcast it's current assigned value across the rows
        for parent_id in self.pgm.predecessors(node_id):
            if parent_id not in observations_from_parents.columns:
                parent_node = self.pgm.nodes(data='data')[parent_id]
                if not parent_node.metadata.prior:
                    observations_from_parents[parent_id] = parent_node.assignment

        # Exclude parent nodes if requested
        observations_from_parents.drop(exclusions, axis=1, inplace=True)

        # Remove duplicates and creates a new column with the frequency of such duplicates
        observations_from_parents = observations_from_parents.groupby(
            observations_from_parents.columns.values.tolist()).size().reset_index()

        # For each row, create a dictionary containing the assignments and frequency as separate keys
        assignments_and_counts = []
        frequency_column = observations_from_parents.columns[-1]
        for _, series in observations_from_parents.iterrows():
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

    def get_node_frequency(self, node, query, observations):
        if node.get_id() in observations.columns:
            query = {key: value for key, value in query.items() if key in observations.columns}
            observations_given_query = observations.loc[np.all(observations[list(query)] == pd.Series(query), axis=1)]
            frequencies = observations_given_query[node.get_id()].value_counts()
        else:
            frequencies = pd.Series({node.assignment: 1})

        return frequencies

    #     # clock = time.clock()
    #     evidence_data_from_parents = self.get_evidence_data_from_parents(main_node, observations)
    #     evidence_data_from_children_parents = self.get_evidence_data_from_children_parents(main_node, observations)
    #     # print('Elapsed clock time to fill the list of evidence: {} seconds'.format(time.clock() - clock))
    #
    #     posteriors = []
    #     for evidence_from_parents in evidence_data_from_parents:
    #         self.assign_values_to_nodes(evidence_from_parents.to_dict())
    #
    #         # Remove time slice indication
    #         evidence_from_parents_no_time = evidence_from_parents.copy()
    #         if not evidence_from_parents_no_time.empty:
    #             evidence_from_parents_no_time.index = pd.MultiIndex.from_tuples(evidence_from_parents_no_time.index)
    #             evidence_from_parents_no_time = evidence_from_parents_no_time.droplevel(1)
    #
    #         posterior = main_node.cpd.get_distribution(evidence_from_parents_no_time)[0]
    #
    #         child_given_node = [self.pgm.nodes(data='data')[child_node] for child_node in
    #                             self.pgm.successors(main_node.get_id())]
    #         # print('CPD(Child | Node): {}'.format(child_given_node_cpds))
    #         for child_node in child_given_node:
    #             # print('D(Child | Node): {}'.format(cpd.get_distribution(children_assignments)))
    #             evidence_data_from_child_parents = evidence_data_from_children_parents[child_node.metadata.label]
    #
    #             for evidence_from_child_parents in evidence_data_from_child_parents:
    #                 # Remove time slice indication
    #                 evidence_from_child_parents_no_time = evidence_from_child_parents.copy()
    #                 if not evidence_from_child_parents_no_time.empty:
    #                     evidence_from_child_parents_no_time.index = pd.MultiIndex.from_tuples(
    #                         evidence_from_child_parents_no_time.index)
    #                     evidence_from_child_parents_no_time = evidence_from_child_parents_no_time.droplevel(1)
    #                     evidence_from_child_parents_no_time.drop(main_node.metadata.label, inplace=True)
    #
    #                     # Select in the dataframe all the entries with node and cpd.node given that the other nodes have values
    #                     # according to the observations got so far
    #                     # print('Child State: {}'.format(children_assignments[cpd.node.label]))
    #                     # print('Node State: {}'.format(i))
    #                     if child_node.get_id() not in observations.columns:
    #                         query = evidence_from_parents.to_dict()
    #                         query = {key: value for key, value in query.items() if key in observations.columns}
    #                         if query == []:
    #                             # Parent nodes are not in the observations
    #                             matched_evidence = pd.DataFrame([child_node.assignment],
    #                                                             columns=[child_node.metadata.label])
    #                         else:
    #                             observations_slice = observations[list(query.keys())]
    #                             matched_evidence = observations_slice.loc[
    #                                 np.all(observations_slice[list(query)] == pd.Series(query), axis=1)]
    #                             matched_evidence.columns = [label for label, _ in matched_evidence.columns]
    #                             matched_evidence[child_node.metadata.label] = child_node.assignment
    #
    #                     else:
    #                         query = evidence_from_parents.to_dict()
    #                         query.update(evidence_from_child_parents.to_dict())
    #                         query = {key: value for key, value in query.items() if key in observations.columns}
    #                         observations_slice = observations[list(query.keys()) + [child_node.get_id()]]
    #                         matched_evidence = observations_slice.loc[
    #                             np.all(observations_slice[list(query)] == pd.Series(query), axis=1)]
    #                         matched_evidence.columns = [label for label, _ in matched_evidence.columns]
    #
    #                 for child_state, distribution in enumerate(
    #                         child_node.cpd.get_distribution(evidence_from_child_parents_no_time)):
    #                     # Each child node has a distribution for a state of the parent node or the child node has has a distribution which
    #                     # the parameters are generated from the parent node
    #                     try:
    #                         posterior = posterior.mult(distribution,
    #                                                    matched_evidence[child_node.metadata.label].value_counts(),
    #                                                    child_state)
    #                     except TypeError:
    #                         # The instantiation of the parent does not depend on the parameter
    #                         # Eg. node = theta_g3 but i = 0 and d = 0 which yields theta_g0 in g
    #                         pass
    #
    #         posteriors.append(posterior)
    #
    #     final_posterior = None
    #     for posterior in posteriors:
    #         if final_posterior == None:
    #             final_posterior = posterior
    #         else:
    #             final_posterior = final_posterior.add(posterior)
    #
    #     return final_posterior
    #
    # def get_evidence_data_from_parents(self, node, observations):
    #     parent_ids = list(self.pgm.predecessors(node.get_id()))
    #     # Use as evidence the distinct observed values of the parents in the observed data
    #     if parent_ids == [] or not np.any(observations.columns.isin(parent_ids)):
    #         evidence_data_dataframe = pd.DataFrame([])
    #     else:
    #         evidence_data_dataframe = observations.loc[:,
    #                                   observations.columns.isin(parent_ids)].drop_duplicates().reset_index(drop=True)
    #
    #     for parent_id in parent_ids:
    #         if parent_id not in evidence_data_dataframe.columns:
    #             # Use as evidence the previous sampled value for the parent node
    #             assignment = self.pgm.nodes(data='data')[parent_id].assignment
    #
    #             if evidence_data_dataframe.empty:
    #                 evidence_data_dataframe = pd.DataFrame(
    #                     pd.Series({parent_id: assignment}, dtype='object')).transpose()
    #             else:
    #                 number_of_rows = len(evidence_data_dataframe.index)
    #                 evidence_data_dataframe[parent_id] = pd.Series({0: assignment}, dtype='object').repeat(
    #                     number_of_rows).reset_index(drop=True)
    #
    #     if evidence_data_dataframe.empty:
    #         evidence_data = [pd.Series([], dtype='object')]
    #     else:
    #         # Transform the dataframe into a list of series
    #         evidence_data = [row for _, row in evidence_data_dataframe.iterrows()]
    #
    #     return evidence_data
    #
    # def get_evidence_data_from_children_parents(self, node, observations):
    #     evidence_data_per_child = {}
    #     child_ids = list(self.pgm.successors(node.get_id()))
    #
    #     for child_id in child_ids:
    #         child_node = self.pgm.nodes(data='data')[child_id]
    #         evidence_data = self.get_evidence_data_from_parents(child_node, observations)
    #         evidence_data_per_child[child_id[0]] = evidence_data
    #
    #     return evidence_data_per_child
