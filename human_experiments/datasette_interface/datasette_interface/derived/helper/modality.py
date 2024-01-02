from __future__ import annotations

from abc import ABC, abstractmethod

import numpy as np
import pandas as pd
from mne.filter import resample as mne_resample
from scipy.interpolate import interp1d


class ModalityHelper(ABC):
    """
    This class represents an abstract signal modality.
    """

    def __init__(self, original_frequency: int, group_session: str, station: str):
        """
        Creates a modality helper.

        :param original_frequency: original frequency of the signal (hardware frequency).
        :param group_session: group session.
        :param station: station.
        """
        self.original_frequency = original_frequency
        self.group_session = group_session
        self.station = station
        self._data = None

    def is_data_empty(self) -> bool:
        """
        Checks there's any content in the loaded data variable.

        @return: True if empty.
        """

        if self._data is None or len(self._data) == 0:
            return True

        return False

    @abstractmethod
    def has_saved_sync_data(self, target_frequency: int) -> bool:
        """
        Checks whether there's already synchronized signals saved for a group session, station and
        target frequency.

        :param target_frequency: frequency of the synchronized signals.
        """

    @abstractmethod
    def load_data(self):
        """
        Reads modality data to the memory for a specific group session and station.
        """

    @abstractmethod
    def filter(self) -> pd.DataFrame:
        """
        Filters data to remove unwanted artifacts.
        """

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

        up_sampled_data = mne_resample(
            x=self._data.drop(columns="timestamp_unix").values,
            up=up_sample_factor,
            npad="auto",
            axis=0,
        )
        new_timestamps = ModalityHelper._resample_timestamps(
            original_timestamps=self._data["timestamp_unix"].values,
            resampled_size=len(up_sampled_data),
        )

        # Copy data and timestamps to a Data frame
        channel_columns = [c for c in self._data.columns if c != "timestamp_unix"]
        df = pd.DataFrame(data=up_sampled_data, columns=channel_columns)
        df["timestamp_unix"] = new_timestamps

        # Rearrange columns in the same order as the original data.
        self._data = df[self._data.columns]

    @staticmethod
    def _resample_timestamps(
        original_timestamps: np.ndarray, resampled_size: int
    ) -> np.ndarray:
        """
        Performs linear interpolation on original timestamps to find a new list of time stamps on
        the resampled data.

        :param original_timestamps: the list of original numerical timestamps.
        :param resampled_size: the number of samples in the resampled data.

        :return: a list with timestamps of the resampled data.
        """

        seq = np.arange(len(original_timestamps))
        interp_func = interp1d(seq, original_timestamps, kind="linear")
        new_seq = np.linspace(0, seq[-1], resampled_size)
        return interp_func(new_seq)

    def sync_to_clock(self, clock_frequency: float, clock_timestamps: np.ndarray):
        """
        Interpolates signals to find values at times defined by a given start time and frequency.

        :param clock_frequency: frequency of the clock we are interpolating with.
        :param clock_timestamps: timestamps to use for interpolation.
        """

        df = self._data.drop(columns="timestamp_unix").apply(
            func=lambda col: np.interp(
                clock_timestamps, self._data["timestamp_unix"], col
            ),
            axis=0,
        )
        df["frequency"] = clock_frequency
        df["timestamp_unix"] = clock_timestamps

        self._data = df[["frequency"] + list(self._data.columns)]

    @abstractmethod
    def save_synced_data(self):
        """
        Saves synchronized data to the database. It assumes that the function sync_to_clock has
        been called previously.
        """
