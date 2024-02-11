from logging import info
from typing import List

import neurokit2 as nk
import pandas as pd
from sqlalchemy import Engine, func, select
from sqlalchemy.orm import Session

from datasette_interface.common.constants import EEG_FREQUENCY
from datasette_interface.common.utils import convert_unix_timestamp_to_iso8601
from datasette_interface.database.entity.derived.eeg_sync import EEGSync
from datasette_interface.database.entity.derived.ekg_sync import EKGSync
from datasette_interface.database.entity.signal.eeg import EEGRaw
from datasette_interface.derived.helper.modality import ModalityHelper


class EKGHelper(ModalityHelper):
    def __init__(self, group_session: str, station: str, db_engine: Engine):
        """
        Creates an EKG modality helper.

        :param group_session: group session.
        :param station: station.
        :param db_engine: database engine.
        """
        super().__init__(EEG_FREQUENCY, group_session, station, db_engine)

    def get_processed_group_sessions(self, clock_frequency: int) -> List[str]:
        """
        Gets a list of processed group sessions for a specific clock frequency. Processed group
        sessions are those for which there are saved synchronized signals.

        @param clock_frequency: clock frequency.
        @return: list of processed group sessions.
        """
        db = Session(self.db_engine)
        group_sessions = db.scalars(select(EKGSync.group_session_id).distinct()).all()
        db.close()

        return group_sessions

    def has_saved_sync_data(self, target_frequency: int) -> bool:
        """
        Checks whether there's already synchronized GSR saved for a group session, station and
        target frequency.

        :param target_frequency: frequency of the synchronized signals.
        """
        db = Session(self.db_engine)
        num_records = db.scalar(
            select(func.count(EKGSync.id)).where(
                EKGSync.group_session_id == self.group_session,
                EKGSync.frequency == target_frequency,
                EKGSync.station_id == self.group_session,
            )
        )
        db.close()

        return num_records > 0

    def load_data(self):
        """
        Reads GSR data to the memory for a specific group session and station.
        """
        super().load_data()

        query = (
            select(
                EEGRaw.timestamp_unix,
                EEGRaw.aux_ekg,
            )
            .where(
                EEGRaw.group_session_id == self.group_session,
                EEGRaw.station_id == self.station,
            )
            .order_by(EEGRaw.timestamp_unix)
        )
        self._data = pd.read_sql_query(query, self.db_engine)
        self._data = self._data.rename(columns={"aux_ekg": "ekg"})
        self._data["heart_rate"] = 0

    def filter(self) -> pd.DataFrame:
        """
        Filters data to remove unwanted artifacts.
        """
        super().filter()

        pre_processed_df = pd.DataFrame(nk.ecg_process(
                self._data["ekg"].values, sampling_rate=self.original_frequency
            )[0])

        # Copy data and timestamps to a Data frame
        df = pd.DataFrame(
            {
                "ekg": pre_processed_df["ECG_Clean"].values,
                "heart_rate": pre_processed_df["ECG_Rate"].values,
                "timestamp_unix": self._data["timestamp_unix"].values,
            }
        )

        # Rearrange columns in the same order as the original data.
        self._data = df[self._data.columns]

    def save_synced_data(self):
        """
        Saves synchronized EKG data to the database. It assumes that the function sync_to_clock
        has been called previously.
        """
        super().save_synced_data()

        df = self._data.reset_index().rename(columns={"index": "id"})
        df["timestamp_iso8601"] = df["timestamp_unix"].apply(
            convert_unix_timestamp_to_iso8601
        )
        df["group_session_id"] = self.group_session
        df["station_id"] = self.station

        info(f"Converting DataFrame of size {len(df)} rows to records.")
        records = df.to_dict("records")

        db = Session(self.db_engine)
        ekg_data = []
        for record in records:
            ekg_data.append(EKGSync(**record))

        info("Saving to the database.")
        db.add_all(ekg_data)
        db.commit()
        db.close()
