import numpy as np

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