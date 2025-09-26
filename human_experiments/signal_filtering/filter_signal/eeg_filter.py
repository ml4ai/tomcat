import numpy as np

from .filter_creator import notch_filter_creator


def eeg_filter(signal: np.ndarray) -> np.ndarray:
    notch_filter = notch_filter_creator(data_frequency=500,
                                        notch_filter_frequency=[60.0],
                                        notch_width=2.0,
                                        trans_bandwidth=9.0)

    return notch_filter(signal)
