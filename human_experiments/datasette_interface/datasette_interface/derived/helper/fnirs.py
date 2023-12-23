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

    def __init__(self):
        """
        Creates a fNIRS modality helper.
        """
        super().__init__(FNIRS_FREQUENCY)

    def load_data(self, group_session: str):
        """
        Reads fNIRS data to the memory for a specific group session.

        :param group_session: group session.
        """
        super().load_data(group_session)

        db_session = next(get_db())
        self._data = pd.read_sql(
            select(FNIRSRaw).where(FNIRSRaw.group_session_id == group_session),
            db_session)
        db_session.close()

    def filter(self) -> pd.DataFrame:
        """
        Filters data to remove unwanted artifacts.
        """
        super().filter()

        filtered_values = mne_filter_data(self._data.drop(columns=["timestamp_unix"]).values.T,
                                          sfreq=self.original_frequency,
                                          l_freq=FNIRS_LOW_FREQUENCY_THRESHOLD,
                                          h_freq=FNIRS_HIGH_FREQUENCY_THRESHOLD,
                                          method=FNIRS_BANDPASS_FILTER_METHOD).T

        # Copy data and timestamps to a Data frame
        df = pd.DataFrame(data=filtered_values,
                          columns=[c for c in self._data.columns if c != "timestamp_unix"])
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
