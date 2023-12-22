from __future__ import annotations

import math
from abc import ABC, abstractmethod
from typing import List, Optional, Type

from mne.filter import resample as mne_resample
import numpy as np
import pandas as pd
from scipy.interpolate import interp1d

from data_pre_processing.signal.table.base import Base


class Modality(ABC):
    """
    This class represents an abstract data signal modality.
    """

    def __init__(self, name: str):
        """
        Creates a modality.

        :param name: modality name.
        """
        self.name = name

    @staticmethod
    def upsample(data: pd.DataFrame,
                  upsampling_factor: float) -> pd.DataFrame:
        """
        Upsamples a signal up to a factor. This generates a time series of size defined by
        #original samples * upsampling_factor. We use the mne resample function which applies
        the resampling in the frequency domain via FFT and then projects the series back to the
        time domain with IFFT.

        To map the new samples to timestamps, we first perform linear interpolation on the original
        timestamps and use that to estimate the timestamps of the upsampled data.

        :param data: timestamped signal values. Each row in data must contain one data point. One
            of the columns must be called timestamp_unix, which stores a unix timestamp for each
            data point. The remaining columns are the data channels/features.
        :param upsampling_factor: by how many times to upsample the series.
        :return: data frame with upsampled data with the same columns as the original data and
            more rows of data.
        """

        if upsampling_factor == 1:
            return data

        if not data["timestamp_unix"].is_monotonic_increasing:
            raise ValueError("timestamp_unix column must be sorted in ascending order.")

        upsampled_data = mne_resample(x=data.drop(columns="timestamp_unix").values,
                                      up=upsampling_factor,
                                      npad="auto",
                                      axis=0)
        timestamps = Modality._resample_timestamps(
            original_timestamps=data["timestamp_unix"].values,
            resampled_size=len(upsampled_data)
        )

        # Copy data and timestamps to a Data frame
        df = pd.DataFrame(data=upsampled_data,
                          columns=[c for c in data.columns if c != "timestamp_unix"])
        df["timestamp_unix"] = timestamps

        # Rearrange columns in the same order as the original data.
        return df[data.columns]

    @staticmethod
    def downsample(data: pd.DataFrame,
                   downsampling_factor: int) -> pd.DataFrame:
        """
        Downsampled a signal up to a factor. This generates a time series of size defined by
        #original samples / downsampling_factor. We use the mne resample function which applies
        the resampling in the frequency domain via FFT and then projects the series back to the
        time domain with IFFT.

        Note: Raw -> upsample -> downsample does not perfectly recover the raw data because the
        downsample operation uses a low-pass filtering to avoid aliasing, which may slightly change
        the values of the upsampled data.

        To map the new samples to timestamps, we first perform linear interpolation on the original
        timestamps and use that to estimate the timestamps of the upsampled data.

        :param data: timestamped signal values. Each row in data must contain one data point. One
            of the columns must be called timestamp_unix, which stores a unix timestamp for each
            data point. The remaining columns are the data channels/features.
        :param upsampling_factor: by how many times to upsample the series.
        :return: data frame with upsampled data with the same columns as the original data and
            more rows of data.
        """

        if downsampling_factor == 1:
            return data

        if not data["timestamp_unix"].is_monotonic_increasing:
            raise ValueError("timestamp_unix column must be sorted in ascending order.")

        upsampled_data = mne_resample(x=data.drop(columns="timestamp_unix").values,
                                      down=downsampling_factor,
                                      npad="auto",
                                      axis=0)
        timestamps = Modality._resample_timestamps(
            original_timestamps=data["timestamp_unix"].values,
            resampled_size=len(upsampled_data)
        )

        # Copy data and timestamps to a Data frame
        df = pd.DataFrame(data=upsampled_data,
                          columns=[c for c in data.columns if c != "timestamp_unix"])
        df["timestamp_unix"] = timestamps

        # Rearrange columns in the same order as the original data.
        return df[data.columns]

    @staticmethod
    def _resample_timestamps(original_timestamps: np.ndarray, resampled_size: int):
        """
        Performs linear interpolation on original timestamps to find a new list of time stamps on
        the resampled data. The size of the resampled data can be either larger (upsampling) or
        smaller (downsampling) than the original data.

        :param original_timestamps: the list of original numerical timestamps.
        :param resampled_size: the number of samples in the resampled data.

        :return: a list with timestamps of the resampled data.
        """

        seq = np.arange(len(original_timestamps))
        interp_func = interp1d(seq, original_timestamps, kind='linear')
        new_seq = np.linspace(0, seq[-1], resampled_size)
        return interp_func(new_seq)

    @staticmethod
    def interpolate(data: pd.DataFrame,
                    target_frequency: float,
                    start_time: Optional[float] = None,
                    end_time: Optional[float] = None) -> pd.DataFrame:
        """
        Interpolates signals to find values at times defined by a given start time and frequency.

        :param data: timestamped signal values. Each row in data must contain one data point. One
            of the columns must be called timestamp_unix, which stores a unix timestamp for each
            data point. The remaining columns are the data channels/features.
        :param target_frequency: frequency of the final series.
        :param start_time: timestamp of the initial point. If not entered, the first timestamp of
            the data will be used as starting point.
        :param end_time: timestamp of the final point. If not entered, the last timestamp of
            the data will be used as ending point.
        :return: data frame with evenly spaced signal values in time at a certain frequency.
        """

        if not data["timestamp_unix"].is_monotonic_increasing:
            raise ValueError("timestamp_unix column must be sorted in ascending order.")

        if start_time is None:
            start_time = data["timestamp_unix"].min()
        else:
            if start_time < data["timestamp_unix"].min():
                raise ValueError("start_time must be >= df['timestamp_unix'].min()")
            if start_time > data["timestamp_unix"].max():
                raise ValueError("start_time must be <= df['timestamp_unix'].max()")

        if end_time is None:
            end_time = data["timestamp_unix"].max()
        else:
            if end_time < data["timestamp_unix"].min():
                raise ValueError("end_time must be >= df['timestamp_unix'].min()")
            if end_time > data["timestamp_unix"].max():
                raise ValueError("end_time must be <= df['timestamp_unix'].max()")

        # The arange function is not numerically stable with small float numbers. So we use the
        # number of points to create a sequence and scale the sequence by the inverse of the
        # frequency.
        num_time_points = math.floor((end_time - start_time) * target_frequency) + 1
        evenly_spaced_timestamps = start_time + np.arange(num_time_points) / target_frequency

        df = data.drop(columns=["timestamp_unix"]).apply(
            func=lambda col: np.interp(evenly_spaced_timestamps, data["timestamp_unix"], col),
            axis=0
        )
        df["timestamp_unix"] = evenly_spaced_timestamps

        # Rearrange columns in the same order as the original data.
        return df[data.columns]

    @abstractmethod
    def get_data_mode_table_class(self, data_mode: str) -> Type[Base]:
        """
        Gets table class for a specific data mode.

        :param data_mode: one of ["unfiltered", "filtered"], indicating synchronized unfiltered
            and filtered data tables.
        :return class of the table.
        """
        pass

    @abstractmethod
    def filter(self, data: pd.DataFrame, data_frequency: int) -> pd.DataFrame:
        """
        Filters data to remove unwanted artifacts.

        :param data: timestamped signal values. Each row in data must contain one data point. One
            of the columns must be called timestamp_unix, which stores a unix timestamp for each
            data point. The remaining columns are the data channels/features.
        :param data_frequency: frequency of the signal.

        :return: filtered data in the same format as the original.
        """
        pass

    @property
    @abstractmethod
    def channels(self) -> List[str]:
        """
        Gets the list of channels of the signal data.

        :return: list of channel names.
        """
        pass

    @abstractmethod
    def read_data(self, group_session: str) -> pd.DataFrame:
        """
        Reads data from a group session from the database.
        """
        pass
