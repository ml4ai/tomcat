import re
import numpy as np
import pandas as pd


class DataQuery:
    """
    Check the inferred probabilities in a user friendly way
    """

    def __init__(self, samples):
        self.samples = samples

    def aggregate_over_time(self, inference_query, initial_time_slice, final_time_slice):
        pattern_t0 = "([A-Za-z0-9_]+_t0)"
        pattern_t1 = "([A-Za-z0-9_]+_t1)"
        pattern_t = "([A-Za-z0-9_]+_t\*)"
        variables_t0 = set(re.findall(pattern_t0, inference_query))
        variables_t1 = set(re.findall(pattern_t1, inference_query))
        variables_t = set(re.findall(pattern_t, inference_query))

        tables = []
        for t in range(initial_time_slice, final_time_slice + 1):
            if t == final_time_slice:
                if len(variables_t) == 0:
                    break
                else:
                    query = inference_query.replace('t*', str(t))
            else:
                query = inference_query.replace('t0', str(t)).replace('t1', str(t + 1)).replace('t*', str(t))

            table = self.infer_from_samples(query)
            columns = list(table.columns)
            for i, column in enumerate(columns.copy()):
                if isinstance(column, tuple):
                    if '{}_t0'.format(column[0]) in variables_t0 or '{}_t1'.format(column[0]) in variables_t1:
                        if column[1] == t:
                            columns[i] = (column[0], 't')
                        else:
                            columns[i] = (column[0], 't+1')
                    elif '{}_t*'.format(column[0]) in variables_t:
                        columns[i] = (column[0], 't')

            table.columns = columns
            tables.append(table)

        table = pd.concat(tables)
        table = table.groupby(list(table.columns[:-1]), as_index=False).mean()
        # For marginals we need to normalize the frequencies
        table['Frequency'] = table['Frequency']/sum(table['Frequency'])
        return table

    def infer_from_samples(self, inference_query):
        pattern = re.compile(r"""p\(
                    (?P<latent>([A-Za-z0-9_]+_[0-9]+(=[0-9]+)?,)*[A-Za-z0-9_]+_[0-9]+(=[0-9]+)?)
                    (\|
                    (?P<evidence>([A-Za-z0-9_]+_[0-9]+=[0-9]+,)*[A-Za-z0-9_]+_[0-9]+=[0-9]+)
                    )?\)""", re.VERBOSE)
        match = pattern.match(inference_query.replace(' ', ''))
        latent_assignments = self.get_assignments_per_query(match.group('latent'))
        evidence_assignments = self.get_assignments_per_query(match.group('evidence'))

        unmarginalized_variables = list(latent_assignments.keys()) + list(evidence_assignments.keys())
        inference_table = self.samples[unmarginalized_variables]  # Remove other variables' assignments from the samples
        inference_table = self.get_frequency_by_assignments(inference_table, evidence_assignments, normalize=True)
        inference_table = self.select_data_by_assignments(inference_table, latent_assignments)

        return inference_table

    def get_assignments_per_query(self, assignment_string):
        assignments = {}
        if assignment_string != None:
            pattern = re.compile('(?P<variable>[A-Za-z0-9_]+)_(?P<time_slice>[0-9]+)(=(?P<value>[0-9]+))?', re.VERBOSE)
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

    def get_joint_frequencies(self, ascending=False, normalize=True, columns=[]):
        joint_table = self.samples.groupby(self.samples.columns.values.tolist()).size().reset_index()
        new_column_names = list(joint_table.columns)
        new_column_names[-1] = 'Frequency'
        joint_table.columns = new_column_names

        if normalize:
            joint_table['Frequency'] = joint_table['Frequency'] / joint_table['Frequency'].sum()

        if columns != []:
            new_column_names[:-1] = columns
            joint_table = joint_table[new_column_names]

        joint_table = joint_table.sort_values(by=['Frequency'], ascending=ascending).reset_index(drop=True)

        return joint_table
