from __future__ import annotations

import math
from abc import ABC, abstractmethod
from typing import List, Optional, Type

from mne.filter import resample as mne_resample
import numpy as np
import pandas as pd
from scipy.interpolate import interp1d

from data_pre_processing.signal.table.base import Base


class ModalityHelper(ABC):
    """
    This class represents an abstract signal modality.
    """

    def __init__(self, original_frequency: int):
        """
        Creates a modality.

        :param original_frequency: original frequency of the signal (hardware frequency).
        """
        self.original_frequency = original_frequency
        self._data = None

    @abstractmethod
    def load_data(self, group_session: str):
        """
        Reads modality data to the memory for a specific group session.

        :param group_session: group session.
        """
        pass

    @abstractmethod
    def filter(self) -> pd.DataFrame:
        """
        Filters data to remove unwanted artifacts.
        """
        pass

    def up_sample(self, up_sample_factor: float):
        """
        Up-samples a signal up to a factor. This generates a time series of size defined by
        number of original samples * up_sample_factor. We use the MNE resample function which
        applies the resampling in the frequency domain via FFT and then projects the series back
        to the time domain with IFFT. This assumes the samples are equally spaced (or
        approximately so).

        :param up_sample_factor: by how many times to up-sample the series.
        """

        if up_sample_factor == 1:
            return

        up_sampled_data = mne_resample(x=self._data.drop(columns="timestamp_unix").values,
                                       up=upsampling_factor,
                                       npad="auto",
                                       axis=0)
        timestamps = ModalityHelper._resample_timestamps(
            original_timestamps=data["timestamp_unix"].values,
            resampled_size=len(upsampled_data)
        )

        # Copy data and timestamps to a Data frame
        df = pd.DataFrame(data=up_sampled_data,
                          columns=[c for c in data.columns if c != "timestamp_unix"])
        df["timestamp_unix"] = timestamps

        # Rearrange columns in the same order as the original data.
        self._data = df[self._data.columns]

    @staticmethod
    def _resample_timestamps(original_timestamps: np.ndarray, resampled_size: int) -> np.ndarray:
        """
        Performs linear interpolation on original timestamps to find a new list of time stamps on
        the resampled data.

        :param original_timestamps: the list of original numerical timestamps.
        :param resampled_size: the number of samples in the resampled data.

        :return: a list with timestamps of the resampled data.
        """

        seq = np.arange(len(original_timestamps))
        interp_func = interp1d(seq, original_timestamps, kind='linear')
        new_seq = np.linspace(0, seq[-1], resampled_size)
        return interp_func(new_seq)

    def sync_to_clock(self, clock_frequency: float, clock_timestamps: np.ndarray):
        """
        Interpolates signals to find values at times defined by a given start time and frequency.

        :param clock_frequency: frequency of the clock we are interpolating with.
        :param clock_timestamps: timestamps to use for interpolation.
        """

        df = self._data.drop(columns=["timestamp_unix"]).apply(
            func=lambda col: np.interp(clock_timestamps, self._data["timestamp_unix"], col),
            axis=0
        )
        df["frequency"] = clock_frequency
        df["timestamp_unix"] = clock_timestamps

        self._data = df

    @abstractmethod
    def save_synced_data(self):
        """
        Saves synchronized data to the database. It assumes that the function sync_to_clock has
        been called previously.
        """
        pass
