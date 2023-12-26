from typing import List, Type, Union

from mne.filter import filter_data as mne_filter_data
import pandas as pd

from datasette_interface.derived.helper.modality import ModalityHelper
# from datasette_interface.signal.table.fnirs import FNIRSSyncUnfiltered, FNIRSSyncFiltered
from sqlalchemy import select
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.signal.eeg import EEGRaw
from datasette_interface.common.constants import (EEG_FREQUENCY,
                                                  EEG_NOTCH_FILTER_FREQUENCY,
                                                  EEG_NOTCH_WIDTH,
                                                  EEG_TRANSISION_BANDWIDTH)


class EEGHelper(ModalityHelper):

    def __init__(self, group_session: str, station: str):
        """
        Creates an EEG modality helper.

        :param group_session: group session.
        :param station: station.
        """
        super().__init__(EEG_FREQUENCY, group_session, station)

    def has_saved_sync_data(self, target_frequency: int) -> bool:
        """
        Checks whether there's already synchronized EEG saved for a group session, station and
        target frequency.

        :param target_frequency: frequency of the synchronized signals.
        """
        db = next(get_db())
        num_records = db.scalar(
            select(func.count(EEGSync)).where(EEGSync.group_session_id == self.group_session,
                                                EEGSync.frequency == target_frequency,
                                                EEGSync.station_id == self.group_session))
        db.close()

        return num_records > 0

    def load_data(self):
        """
        Reads EEG data to the memory for a specific group session and station.
        """
        super().load_data()

        db_session = next(get_db())
        self._data = pd.read_sql(
            select(EEGRaw).where(
                EEGRaw.group_session_id == self.group_session,
                EEGRaw.station_id == self.station),
            db_session)
        # New IDs will be set later when the data is saved as the number of samples may change,
        self._data = self._data.drop(columns=["id", "timestamp_iso8601"])
        db_session.close()

    def filter(self) -> pd.DataFrame:
        """
        Filters data to remove unwanted artifacts.
        """
        super().filter()

        filtered_values = np.array(
            notch_filter(self._data.drop(columns="timestamp_unix").values.T,
                         Fs=self.original_frequency,
                         freqs=EEG_NOTCH_FILTER_FREQUENCY,
                         notch_widths=EEG_NOTCH_WIDTH,
                         trans_bandwidth=EEG_TRANSISION_BANDWIDTH)).T

        # Copy data and timestamps to a Data frame
        df = pd.DataFrame(data=filtered_values,
                          columns=[c for c in self._data.columns if c != "timestamp_unix"])
        df["timestamp_unix"] = self._data["timestamp_unix"]

        # Rearrange columns in the same order as the original data.
        self._data = df[self._data.columns]

    def save_synced_data(self):
        """
        Saves synchronized EEG data to the database. It assumes that the function sync_to_clock
        has been called previously.
        """
        super().save_synced_data()

        # TODO: Implement this: save eeg, gsr and ekg in separate tables.
