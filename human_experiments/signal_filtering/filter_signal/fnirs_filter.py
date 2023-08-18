import numpy as np

from .filter_creator import bandpass_filter_creator


def fnirs_filter(signal: np.ndarray) -> np.ndarray:
    bandpass_filter = bandpass_filter_creator(data_frequency=10,
                                              low_cut=0.01,
                                              high_cut=0.2)

    return bandpass_filter(signal)
