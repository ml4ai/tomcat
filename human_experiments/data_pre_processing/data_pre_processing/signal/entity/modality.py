from __future__ import annotations
from abc import ABC, abstractmethod
from typing import List, Optional, Type

from mne.filter import resample
import numpy as np
import pandas as pd


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

    def upsample(self,
                 data: pd.DataFrame,
                 upsampling_factor: int) -> pd.DataFrame:
        """
        Upsamples signals to by an integer factor. We upsample using an integer factor so we can
        align the new samples to raw timestamps. (upsampling_factor - 1) new samples will be
        created between each subsequent points. Because the samples are equally spaced and because
        we know the timestamps of the original points, we can estimate the timestamp of the new
        samples. If the upsampling_factor was a float number, this algorithm wouldn't work as the
        number of points created between each two subsequent points wouldn't be well defined.

        :param data: timestamped signal values. Each row in data must contain one data point. One
            of the columns must be called timestamp_unix, which stores a unix timestamp for each
            data point. The remaining columns are the data channels/features.
        :param upsampling_factor: by how many times to upsample the series.
        :return: data frame with upsampled data with the same columns as the original data and
            more rows of data.
        """

        if not data["timestamp_unix"].is_monotonic_increasing:
            raise ValueError("timestamp_unix column must be sorted in ascending order.")

        upsampled_data = resample(x=data.drop(columns="timestamp_unix").values,
                                  up=upsampling_factor,
                                  npad="auto",
                                  axis=0)

        # Creates a list of timestamps to fit the samples. upsampling_factor -1 samples are created
        # between two original points. We attribute these samples to evenly spaced timestamps
        # between the timestamps of the original points.
        timestamps = []
        for i in range(len(data) - 1):
            start_value = data.iloc[i]["timestamp_unix"]
            end_value = data.iloc[i + 1]["timestamp_unix"]
            dt = (end_value - start_value) / upsampling_factor

            # Generate evenly spaced timestamps between start and end for n-1 points
            evenly_spaced_timestamps = np.arange(start_value, end_value, dt)

            # Add these values to the new_rows list
            timestamps.extend(evenly_spaced_timestamps)
        timestamps.append(data.iloc[-1]["timestamp_unix"])

        # Remove samples generated after the final point because we cannot estimate their
        # timestamps.
        upsampled_data = upsampled_data[:-(upsampling_factor - 1), :]

        df = pd.DataFrame(data=upsampled_data,
                          columns=[c for c in data.columns if c != "timestamp_unix"])
        df["timestamp_unix"] = timestamps

        # Rearrange columns in the same order as the original data.
        return df[data.columns]

    def interpolate(self,
                    data: pd.DataFrame,
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

        # np.arange is not numerically stable when working with high frequency data like EEG.
        # So we manually fill the array of evenly paced time points.
        evenly_spaced_timestamps = [start_time]
        step_size = 1 / target_frequency
        while evenly_spaced_timestamps[-1] < end_time:
            evenly_spaced_timestamps.append(evenly_spaced_timestamps[-1] + step_size)
        evenly_spaced_timestamps = np.array(evenly_spaced_timestamps)

        df = data.drop(columns=["timestamp_unix"]).apply(
            lambda col: np.interp(evenly_spaced_timestamps, data['timestamp_unix'], col)
        )
        df['timestamp_unix'] = evenly_spaced_timestamps

        # Rearrange columns in the same order as the original data.
        return df[data.columns]

    @abstractmethod
    def get_data_mode_table_class(self, data_mode: str) -> Type[Base]:
        """
        Gets table class for a specific data mode.

        :param data_mode: one of ["sync", "filtered"], indicating synchronized and filtered data
            tables.
        :return class of the table.
        """
        pass

    @abstractmethod
    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        pass

    @property
    @abstractmethod
    def channels(self) -> List[str]:
        pass
