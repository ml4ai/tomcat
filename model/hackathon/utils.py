import numpy as np
import datetime
import json

ZERO = 10 ** -16

def log(values):
    values[values == 0] = ZERO
    return np.log(values)

def normalize_log_array(log_array):
    log_array -= np.max(log_array)
    log_array = np.exp(log_array)
    return log_array / np.sum(log_array)

def normalize_log_matrix_columnwise(log_matrix):
    log_matrix -= np.max(log_matrix, axis=1).reshape(-1, 1)
    log_matrix = np.exp(log_matrix)
    return log_matrix / np.sum(log_matrix, axis=1).reshape(-1, 1)

def to_datetime(timestamp_string):
    return datetime.datetime.strptime(timestamp_string, '%Y-%m-%dT%H:%M:%S.%fZ')

def read_data_from_file(filepath):
    """
    This function reads data from the testbed saved into a file
    """
    data = []
    with open(filepath, 'r') as file:
        for json_obj in file:
            data.append(json.loads(json_obj))

    return data