from typing import List, Type, Union

from mne.filter import filter_data as mne_filter_data
import pandas as pd

from data_pre_processing.signal.common.constants import FNIRS_LOW_FREQUENCY_THRESHOLD, \
    FNIRS_HIGH_FREQUENCY_THRESHOLD, FNIRS_BANDPASS_FILTER_METHOD
from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.signal.table.fnirs import FNIRSSyncUnfiltered, FNIRSSyncFiltered
from sqlalchemy import select
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.signal.fnirs import FNIRSRaw


class FNIRS(Modality):

    def __init__(self,
                 low_frequency_threshold: float = FNIRS_LOW_FREQUENCY_THRESHOLD,
                 high_frequency_threshold: float = FNIRS_HIGH_FREQUENCY_THRESHOLD,
                 bandpass_filter_method: str = FNIRS_BANDPASS_FILTER_METHOD):
        """
        Creates an fNIRS modality.
        """
        super().__init__("fnirs")
        self.low_frequency_threshold = low_frequency_threshold
        self.high_frequency_threshold = high_frequency_threshold
        self.bandpass_filter_method = bandpass_filter_method

    def read_data(self, group_session: str) -> pd.DataFrame:
        db_session = next(get_db())
        return pd.read_sql(
            select(FNIRSRaw).where(FNIRSRaw.group_session_id == group_session),
            db_session)

    @property
    def channels(self) -> List[str]:
        """
        Gets list of fNIRS channels.

        :return: list of fNIRS channels.
        """
        return CHANNELS

    def get_data_mode_table_class(self, data_mode: str) -> Type[
        Union[FNIRSSyncUnfiltered, FNIRSSyncFiltered]]:
        """
        Gets table class for a specific data mode.

        :param data_mode: one of ["unfiltered", "filtered"], indicating synchronized and filtered
            data tables.
        :return class of the table.
        """

        if data_mode == "unfiltered":
            return FNIRSSyncUnfiltered

        if data_mode == "filtered":
            return FNIRSSyncFiltered

        raise ValueError(f"There's no table class for data_mode ({data_mode}).")

    def filter(self, data: pd.DataFrame, data_frequency: int) -> pd.DataFrame:
        """
        Filters fNIRS to remove unwanted artifacts like breathing and heart beat by applying a
        bandpass filter.

        :param data: timestamped signal values. Each row in data must contain one data point. One
            of the columns must be called timestamp_unix, which stores a unix timestamp for each
            data point. The remaining columns are the data channels/features.
        :param data_frequency: frequency of the signal.

        :return: filtered data in the same format as the original.
        """
        # Transpose values because the filter_data function in MNE assumes the time dimension is
        # the last one.
        filtered_values = mne_filter_data(data.drop(columns=["timestamp_unix"]).values.T,
                                          sfreq=data_frequency,
                                          l_freq=self.low_frequency_threshold,
                                          h_freq=self.high_frequency_threshold,
                                          method=self.bandpass_filter_method).T

        # Copy data and timestamps to a Data frame
        df = pd.DataFrame(data=filtered_values,
                          columns=[c for c in data.columns if c != "timestamp_unix"])
        df["timestamp_unix"] = data["timestamp_unix"]

        # Rearrange columns in the same order as the original data.
        return df[data.columns]


CHANNELS = [
    "s1_d1_hbo",
    "s1_d2_hbo",
    "s2_d1_hbo",
    "s2_d3_hbo",
    "s3_d1_hbo",
    "s3_d3_hbo",
    "s3_d4_hbo",
    "s4_d2_hbo",
    "s4_d4_hbo",
    "s4_d5_hbo",
    "s5_d3_hbo",
    "s5_d4_hbo",
    "s5_d6_hbo",
    "s6_d4_hbo",
    "s6_d6_hbo",
    "s6_d7_hbo",
    "s7_d5_hbo",
    "s7_d7_hbo",
    "s8_d6_hbo",
    "s8_d7_hbo",
    "s1_d1_hbr",
    "s1_d2_hbr",
    "s2_d1_hbr",
    "s2_d3_hbr",
    "s3_d1_hbr",
    "s3_d3_hbr",
    "s3_d4_hbr",
    "s4_d2_hbr",
    "s4_d4_hbr",
    "s4_d5_hbr",
    "s5_d3_hbr",
    "s5_d4_hbr",
    "s5_d6_hbr",
    "s6_d4_hbr",
    "s6_d6_hbr",
    "s6_d7_hbr",
    "s7_d5_hbr",
    "s7_d7_hbr",
    "s8_d6_hbr",
    "s8_d7_hbr"
]
