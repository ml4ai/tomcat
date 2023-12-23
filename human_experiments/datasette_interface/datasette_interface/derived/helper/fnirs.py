from typing import List, Type, Union

from mne.filter import filter_data as mne_filter_data
import pandas as pd

from data_pre_processing.signal.common.constants import FNIRS_LOW_FREQUENCY_THRESHOLD, \
    FNIRS_HIGH_FREQUENCY_THRESHOLD, FNIRS_BANDPASS_FILTER_METHOD
from datasette_interface.derived.helper.modality import ModalityHelper
# from datasette_interface.signal.table.fnirs import FNIRSSyncUnfiltered, FNIRSSyncFiltered
from sqlalchemy import select
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.signal.fnirs import FNIRSRaw
from datasette_interface.common.constants import (FNIRS_FREQUENCY,
                                                  FNIRS_LOW_FREQUENCY_THRESHOLD,
                                                  FNIRS_HIGH_FREQUENCY_THRESHOLD,
                                                  FNIRS_BANDPASS_FILTER_METHOD)


class FNIRSHelper(ModalityHelper):

    def __init__(self, group_session: str, station: str):
        """
        Creates an fNIRS modality helper.

        :param group_session: group session.
        :param station: station.
        """
        super().__init__(FNIRS_FREQUENCY, group_session, station)

    def load_data(self):
        """
        Reads fNIRS data to the memory for a specific group session and station.
        """
        super().load_data()

        db_session = next(get_db())
        self._data = pd.read_sql(
            select(FNIRSRaw).where(
                FNIRSRaw.group_session_id == self.group_session,
                FNIRSRaw.station_id == self.station),
            db_session)
        # New IDs will be set later when the data is saved as the number of samples may change,
        self._data = self._data.drop(columns=["id"])
        db_session.close()

    def filter(self) -> pd.DataFrame:
        """
        Filters data to remove unwanted artifacts.
        """
        super().filter()

        filtered_values = mne_filter_data(
            self._data.drop(columns="timestamp_unix").values.T,
            sfreq=self.original_frequency,
            l_freq=FNIRS_LOW_FREQUENCY_THRESHOLD,
            h_freq=FNIRS_HIGH_FREQUENCY_THRESHOLD,
            method=FNIRS_BANDPASS_FILTER_METHOD).T

        # Copy data and timestamps to a Data frame
        channel_columns = [c for c in data.columns if c != "timestamp_unix"]
        df = pd.DataFrame(data=filtered_values,
                          columns=channel_columns)
        df["timestamp_unix"] = self._data["timestamp_unix"]

        # Rearrange columns in the same order as the original data.
        self._data = df[self._data.columns]

    def save_synced_data(self):
        """
        Saves synchronized fNIRS data to the database. It assumes that the function sync_to_clock
        has been called previously.
        """
        super().save_synced_data()

        # TODO: Implement this
