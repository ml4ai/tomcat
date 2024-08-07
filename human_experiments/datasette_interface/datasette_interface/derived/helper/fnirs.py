from logging import info
from typing import List

import pandas as pd
from mne.filter import filter_data as mne_filter_data
from sqlalchemy import Engine, func, select
from sqlalchemy.orm import Session

from datasette_interface.common.constants import (
    FNIRS_BANDPASS_FILTER_METHOD,
    FNIRS_FREQUENCY,
    FNIRS_HIGH_FREQUENCY_THRESHOLD,
    FNIRS_LOW_FREQUENCY_THRESHOLD,
)
from datasette_interface.common.utils import convert_unix_timestamp_to_iso8601
from datasette_interface.database.entity.derived.fnirs_sync import FNIRSSync
from datasette_interface.database.entity.signal.fnirs import FNIRSRaw
from datasette_interface.derived.helper.modality import ModalityHelper


class FNIRSHelper(ModalityHelper):
    def __init__(self, group_session: str, station: str, db_engine: Engine):
        """
        Creates an fNIRS modality helper.

        :param group_session: group session.
        :param station: station.
        :param db_engine: database engine.
        """
        super().__init__(FNIRS_FREQUENCY, group_session, station, db_engine)

    def get_processed_group_sessions(self, clock_frequency: int) -> List[str]:
        """
        Gets a list of processed group sessions for a specific clock frequency. Processed group
        sessions are those for which there are saved synchronized signals.

        @param clock_frequency: clock frequency.
        @return: list of processed group sessions.
        """
        db = Session(self.db_engine)
        group_sessions = db.scalars(select(FNIRSSync.group_session_id).distinct()).all()
        db.close()

        return group_sessions

    def has_saved_sync_data(self, target_frequency: int) -> bool:
        """
        Checks whether there's already synchronized fNIRS saved for a group session, station and
        target frequency.

        :param target_frequency: frequency of the synchronized signals.
        """
        db = Session(self.db_engine)
        num_records = db.scalar(
            select(func.count(FNIRSSync.id)).where(
                FNIRSSync.group_session_id == self.group_session,
                FNIRSSync.frequency == target_frequency,
                FNIRSSync.station_id == self.station,
            )
        )
        db.close()

        return num_records > 0

    def load_data(self):
        """
        Reads fNIRS data to the memory for a specific group session and station.
        """
        super().load_data()

        query = (
            select(
                FNIRSRaw.timestamp_unix,
                FNIRSRaw.s1_d1_hbo,
                FNIRSRaw.s1_d2_hbo,
                FNIRSRaw.s2_d1_hbo,
                FNIRSRaw.s2_d3_hbo,
                FNIRSRaw.s3_d1_hbo,
                FNIRSRaw.s3_d3_hbo,
                FNIRSRaw.s3_d4_hbo,
                FNIRSRaw.s4_d2_hbo,
                FNIRSRaw.s4_d4_hbo,
                FNIRSRaw.s4_d5_hbo,
                FNIRSRaw.s5_d3_hbo,
                FNIRSRaw.s5_d4_hbo,
                FNIRSRaw.s5_d6_hbo,
                FNIRSRaw.s6_d4_hbo,
                FNIRSRaw.s6_d6_hbo,
                FNIRSRaw.s6_d7_hbo,
                FNIRSRaw.s7_d5_hbo,
                FNIRSRaw.s7_d7_hbo,
                FNIRSRaw.s8_d6_hbo,
                FNIRSRaw.s8_d7_hbo,
                FNIRSRaw.s1_d1_hbr,
                FNIRSRaw.s1_d2_hbr,
                FNIRSRaw.s2_d1_hbr,
                FNIRSRaw.s2_d3_hbr,
                FNIRSRaw.s3_d1_hbr,
                FNIRSRaw.s3_d3_hbr,
                FNIRSRaw.s3_d4_hbr,
                FNIRSRaw.s4_d2_hbr,
                FNIRSRaw.s4_d4_hbr,
                FNIRSRaw.s4_d5_hbr,
                FNIRSRaw.s5_d3_hbr,
                FNIRSRaw.s5_d4_hbr,
                FNIRSRaw.s5_d6_hbr,
                FNIRSRaw.s6_d4_hbr,
                FNIRSRaw.s6_d6_hbr,
                FNIRSRaw.s6_d7_hbr,
                FNIRSRaw.s7_d5_hbr,
                FNIRSRaw.s7_d7_hbr,
                FNIRSRaw.s8_d6_hbr,
                FNIRSRaw.s8_d7_hbr,
            )
            .where(
                FNIRSRaw.group_session_id == self.group_session,
                FNIRSRaw.station_id == self.station,
            )
            .order_by(FNIRSRaw.timestamp_unix)
        )
        self._data = pd.read_sql_query(query, self.db_engine)
        info(f"{len(self._data)} rows of data loaded.")

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
            method=FNIRS_BANDPASS_FILTER_METHOD,
        ).T

        # Copy data and timestamps to a Data frame
        channel_columns = [c for c in self._data.columns if c != "timestamp_unix"]
        df = pd.DataFrame(data=filtered_values, columns=channel_columns)
        df["timestamp_unix"] = self._data["timestamp_unix"]

        # Rearrange columns in the same order as the original data.
        self._data = df[self._data.columns]

    def save_synced_data(self):
        """
        Saves synchronized fNIRS data to the database. It assumes that the function sync_to_clock
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
        fnirs_data = []
        for record in records:
            fnirs_data.append(FNIRSSync(**record))

        info("Saving to the database.")
        db.add_all(fnirs_data)
        db.commit()
        db.close()
