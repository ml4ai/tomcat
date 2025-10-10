import numpy as np


def generate_time_series_end_time(start_time: float,
                                  end_time: float,
                                  frequency: float) -> np.ndarray:
    """
    Generate time series with given start and end time
    :param start_time: start time in seconds
    :param end_time: end time in seconds
    :param frequency: sampling frequency in Hz
    :return: time series, excluding any time after the end_time
    """
    num_samples = int((end_time - start_time) * frequency)
    return generate_time_series_num_samples(start_time, num_samples, frequency)


def generate_time_series_num_samples(start_time: float,
                                     num_samples: int,
                                     frequency: float) -> np.ndarray:
    """
    Generate time series with given start time and number of samples
    :param start_time: start time in seconds
    :param num_samples: number of samples
    :param frequency: sampling frequency in Hz
    :return: time series
    """
    return start_time + np.arange(num_samples, dtype=np.int64) / frequency
