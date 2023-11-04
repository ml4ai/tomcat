from typing import List, Type, Union

from mne.filter import notch_filter
import numpy as np
import pandas as pd

from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.signal.table.eeg import EEGSyncUnfiltered, EEGSyncFiltered
from data_pre_processing.signal.common.constants import (EEG_NOTCH_FILTER_FREQUENCY,
                                                         EEG_NOTCH_WIDTH,
                                                         EEG_TRANSISION_BANDWIDTH)


class EEG(Modality):

    def __init__(self,
                 notch_filter_frequency: float = EEG_NOTCH_FILTER_FREQUENCY,
                 notch_width: float = EEG_NOTCH_WIDTH,
                 transition_bandwidth: float = EEG_TRANSISION_BANDWIDTH):
        """
        Creates an EEG modality.
        """
        super().__init__("eeg")

        self.notch_filter_frequency = notch_filter_frequency
        self.notch_width = notch_width
        self.transition_bandwidth = transition_bandwidth

    @property
    def channels(self) -> List[str]:
        """
        Gets list of EEG channels.

        :return: list of EEG channels.
        """
        return CHANNELS

    def get_data_mode_table_class(
            self,
            data_mode: str) -> Type[Union[EEGSyncUnfiltered, EEGSyncFiltered]]:
        """
        Gets table class for a specific data mode.

        :param data_mode: one of ["sync", "filtered"], indicating synchronized and filtered data
            tables.
        :return class of the table.
        """

        if data_mode == "sync":
            return EEGSyncUnfiltered

        if data_mode == "filtered":
            return EEGSyncFiltered

        raise ValueError(f"There's no table class for data_mode ({data_mode}).")

    def filter(self, data: pd.DataFrame, data_frequency: int) -> pd.DataFrame:
        """
        Filters EEG to remove unwanted artifacts like breathing and heart beat by applying a
        bandpass filter.

        :param data: timestamped signal values. Each row in data must contain one data point. One
            of the columns must be called timestamp_unix, which stores a unix timestamp for each
            data point. The remaining columns are the data channels/features.
        :param data_frequency: frequency of the signal.

        :return: filtered data in the same format as the original.
        """
        # Transpose values because the notch_filter function in MNE assumes the time dimension is
        # the last one.
        filtered_values = np.array(notch_filter(data.drop(columns=["timestamp_unix"]).values.T,
                                                Fs=data_frequency,
                                                freqs=self.notch_filter_frequency,
                                                notch_widths=self.notch_width,
                                                trans_bandwidth=self.transition_bandwidth)).T

        # Copy data and timestamps to a Data frame
        df = pd.DataFrame(data=filtered_values,
                          columns=[c for c in data.columns if c != "timestamp_unix"])
        df["timestamp_unix"] = data["timestamp_unix"]

        # Rearrange columns in the same order as the original data.
        return df[data.columns]


CHANNELS = [
    "aff1h",
    "f7",
    "fc5",
    "c3",
    "t7",
    "tp9",
    "pz",
    "p3",
    "p7",
    "o1",
    "o2",
    "p8",
    "p4",
    "tp10",
    "cz",
    "c4",
    "t8",
    "fc6",
    "fcz",
    "f8",
    "aff2h"
]
