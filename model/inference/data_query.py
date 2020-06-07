import re
import numpy as np
import pandas as pd

class DataQuery:
    """
    Check the inferred probabilities in a user friendly way
    """

    def infer_from_samples(self, samples, inference_query):
        pattern = re.compile(r"""p\(
                    (?P<latent>([A-Za-z0-9]+_[0-9]+(=[0-9]+)?,)*[A-Za-z0-9]+_[0-9]+(=[0-9]+)?)
                    \|
                    (?P<evidence>([A-Za-z0-9]+_[0-9]+=[0-9]+,)*[A-Za-z0-9]+_[0-9]+=[0-9]+)
                    \)""", re.VERBOSE)
        match = pattern.match(inference_query.replace(' ', ''))
        latent_assignments = self.get_assignments_per_query(match.group('latent'))
        evidence_assignments = self.get_assignments_per_query(match.group('evidence'))

        unmarginalized_variables = list(latent_assignments.keys()) + list(evidence_assignments.keys())
        inference_table = samples[unmarginalized_variables]  # Remove other variables' assignments from the samples
        inference_table = self.get_frequency_by_assignments(inference_table, evidence_assignments, normalize=True)
        inference_table = self.select_data_by_assignments(inference_table, latent_assignments)

        return inference_table

    def get_assignments_per_query(self, assignment_string):
        assignments = {}
        pattern = re.compile('(?P<variable>[A-Za-z0-9]+)_(?P<time_slice>[0-9]+)(=(?P<value>[0-9]+))?', re.VERBOSE)
        for assignment in assignment_string.split(','):
            match = pattern.match(assignment)
            variable = (match.group('variable'), int(match.group('time_slice')))
            value = match.group('value')
            if value != None:
                if '.' in value:
                    value = float(match.group('value'))
                else:
                    value = int(match.group('value'))
            assignments[variable] = value

        return assignments

    def get_frequency_by_assignments(self, data, assignments, normalize=False):
        data = data.loc[np.all(data[assignments.keys()] == pd.Series(assignments), axis=1)]
        data = data.groupby(data.columns.values.tolist()).size().reset_index()
        columns = data.columns.to_list()
        columns[-1] = 'Frequency'
        data.columns = columns

        if normalize:
            data['Frequency'] = data['Frequency'] / data['Frequency'].sum()

        return data

    def select_data_by_assignments(self, data, assignments):
        valid_assignments = {key: value for key, value in assignments.items() if value != None}
        if valid_assignments == {}:
            return data
        else:
            return data.loc[np.all(data[valid_assignments.keys()] == pd.Series(valid_assignments), axis=1)]

