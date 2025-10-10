from functools import partial

from mne.filter import notch_filter


def notch_filter_creator(data_frequency: float,
                         notch_filter_frequency: float | list[float],
                         notch_width: float,
                         trans_bandwidth: float) -> partial:
    """
    Creates a partial function of the notch_filter function.
    :param data_frequency: The frequency of the input data.
    :param notch_filter_frequency: The frequency of the notch filter.
    :param notch_width: The width of the notch filter.
    :param trans_bandwidth: The transition bandwidth of the notch filter.
    :return: A partial function of the notch_filter function, taking raw data as input.
    """
    partial_notch_filter = partial(notch_filter,
                                   Fs=data_frequency,
                                   freqs=notch_filter_frequency,
                                   notch_widths=notch_width,
                                   trans_bandwidth=trans_bandwidth)
    return partial_notch_filter
