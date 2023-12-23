from typing import List, Type, Union

from mne.filter import filter_data as mne_filter_data
import pandas as pd

from data_pre_processing.signal.common.constants import FNIRS_LOW_FREQUENCY_THRESHOLD, \
    FNIRS_HIGH_FREQUENCY_THRESHOLD, FNIRS_BANDPASS_FILTER_METHOD
from datasette_interface.derived.helper.eeg import EEGHelper
# from datasette_interface.signal.table.fnirs import FNIRSSyncUnfiltered, FNIRSSyncFiltered
from sqlalchemy import select
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.signal.eeg import EEGRaw
from datasette_interface.common.constants import (EEG_FREQUENCY,
                                                  EEG_NOTCH_FILTER_FREQUENCY,
                                                  EEG_NOTCH_WIDTH,
                                                  EEG_TRANSISION_BANDWIDTH)


class EKGHelper(EEGHelper):

    def __init__(self):
        """
        Creates a EKG modality helper.
        """
        super().__init__()

    def load_data(self, group_session: str):
        """
        Reads EKG data to the memory for a specific group session.

        :param group_session: group session.
        """
        db_session = next(get_db())
        self._data = pd.read_sql(
            select(EEGRaw.group_session_id,
                   EEGRaw.station_id,
                   EEGRaw.timestamp_unix,
                   EEGRaw.aux_ekg).where(EEGRaw.group_session_id == group_session),
            db_session)
        db_session.close()

    def save_synced_data(self):
        """
        Saves synchronized EKG data to the database. It assumes that the function sync_to_clock
        has been called previously.
        """
        super().save_synced_data()

        # TODO: Implement this
