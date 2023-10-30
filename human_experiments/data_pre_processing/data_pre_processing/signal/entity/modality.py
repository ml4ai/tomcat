from typing import List
from abc import ABC, abstractmethod

from mne.filter import resample
import numpy as np
import pandas as pd


class Modality(ABC):
    """
    This class represents an abstract data signal modality.
    """

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

        :param data: data with signal values. Each column is one channel and each row one value
            in the time series of data signals.
        :param upsampling_factor: by how many times to upsample the series.
        :return: matrix with upsampled data. One data point per row and one channel per column.
        """

        upsampled_data = resample(x=data.drop(columns="timestamp_unix").values,
                                  up=upsampling_factor,
                                  npad="auto",
                                  axis=0)

        # Create appropriate timestamps
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

        df = pd.DataFrame(upsampled_data[:-(upsampling_factor - 1), :],
                          columns=[c for c in data.columns if c != "timestamp_unix"])
        df["timestamp_unix"] = timestamps

        return df[data.columns]

    def interpolate(self,
                    data: np.array,
                    start_time: float,
                    target_frequency: float) -> pd.DataFrame:
        """
        Interpolates signals

        :param data:
        :param start_time:
        :param target_frequency:
        :return:
        """

        interpolated_signal = linear_interpolation(
            upsampled_values, target_frequency, start_time
        )

    @abstractmethod
    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        pass

    @property
    @abstractmethod
    def table_name(self) -> str:
        pass

    @property
    @abstractmethod
    def channels(self) -> List[str]:
        pass
