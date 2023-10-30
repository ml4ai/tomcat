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
                 data: np.array,
                 source_frequency: float,
                 target_frequency: float) -> np.ndarray:
        """
        Upsamples signals to a target frequency.

        :param data: data with signal values. Each column is one channel and each row one value
            in the time series of data signals.
        :param source_frequency: original (theoretical) frequency of the signal.
        :param target_frequency: desired upsampled frequency of the signal.
        :return: matrix with upsampled data. One data point per row and one channel per column.
        """

        return resample(x=data,
                        up=target_frequency / source_frequency,
                        npad="auto",
                        axis=0)

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

    @abstractmethod
    @property
    def table_name(self) -> str:
        pass

    @abstractmethod
    @property
    def channels(self) -> List[str]:
        pass
