from functools import partial

from mne.filter import filter_data


def bandpass_filter_creator(data_frequency: float,
                            low_cut: float,
                            high_cut: float):
    """
    Creates a partial bandpass filter with IIR method
    :param data_frequency: The frequency of the input data.
    :param low_cut: The lower cut-off frequency of the bandpass filter.
    :param high_cut: The higher cut-off frequency of the bandpass filter.
    :return: A partial function of the filter_data function, taking raw data as input.
    """
    partial_bandpass_filter = partial(filter_data,
                                      sfreq=data_frequency,
                                      l_freq=low_cut,
                                      h_freq=high_cut,
                                      method='iir')
    return partial_bandpass_filter
