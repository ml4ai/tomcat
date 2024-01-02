from logging import info

import numpy as np
import pandas as pd
from mne.filter import notch_filter
from sqlalchemy import Engine, func, select
from sqlalchemy.orm import Session

from datasette_interface.common.constants import (EEG_FREQUENCY,
                                                  EEG_NOTCH_FILTER_FREQUENCY,
                                                  EEG_NOTCH_WIDTH,
                                                  EEG_TRANSISION_BANDWIDTH)
from datasette_interface.common.utils import convert_unix_timestamp_to_iso8601
from datasette_interface.database.entity.derived.eeg_sync import EEGSync
from datasette_interface.database.entity.derived.ekg_sync import EKGSync
from datasette_interface.database.entity.derived.gsr_sync import GSRSync
from datasette_interface.database.entity.signal.eeg import EEGRaw
from datasette_interface.derived.helper.modality import ModalityHelper


class EEGHelper(ModalityHelper):
    def __init__(self, group_session: str, station: str, db_engine: Engine):
        """
        Creates an EEG modality helper.

        :param group_session: group session.
        :param station: station.
        :param db_engine: database engine.
        """
        super().__init__(EEG_FREQUENCY, group_session, station, db_engine)

    def has_saved_sync_data(self, target_frequency: int) -> bool:
        """
        Checks whether there's already synchronized EEG saved for a group session, station and
        target frequency.

        :param target_frequency: frequency of the synchronized signals.
        """
        db = Session(self.db_engine)
        num_records = db.scalar(
            select(func.count(EEGSync.id)).where(
                EEGSync.group_session_id == self.group_session,
                EEGSync.frequency == target_frequency,
                EEGSync.station_id == self.group_session,
            )
        )
        db.close()

        return num_records > 0

    def load_data(self):
        """
        Reads EEG data to the memory for a specific group session and station.
        """
        super().load_data()

        query = (
            select(
                EEGRaw.timestamp_unix,
                EEGRaw.aff1h,
                EEGRaw.f7,
                EEGRaw.fc5,
                EEGRaw.c3,
                EEGRaw.t7,
                EEGRaw.tp9,
                EEGRaw.pz,
                EEGRaw.p3,
                EEGRaw.p7,
                EEGRaw.o1,
                EEGRaw.o2,
                EEGRaw.p8,
                EEGRaw.p4,
                EEGRaw.tp10,
                EEGRaw.cz,
                EEGRaw.c4,
                EEGRaw.t8,
                EEGRaw.fc6,
                EEGRaw.fcz,
                EEGRaw.f8,
                EEGRaw.aff2h,
                EEGRaw.aux_ekg,
                EEGRaw.aux_gsr,
            )
            .where(
                EEGRaw.group_session_id == self.group_session,
                EEGRaw.station_id == self.station,
            )
            .order_by(EEGRaw.timestamp_unix)
        )
        self._data = pd.read_sql_query(query, self.db_engine)

    def filter(self) -> pd.DataFrame:
        """
        Filters data to remove unwanted artifacts.
        """
        super().filter()

        filtered_values = np.array(
            notch_filter(
                self._data.drop(columns="timestamp_unix").values.T,
                Fs=self.original_frequency,
                freqs=EEG_NOTCH_FILTER_FREQUENCY,
                notch_widths=EEG_NOTCH_WIDTH,
                trans_bandwidth=EEG_TRANSISION_BANDWIDTH,
            )
        ).T

        # Copy data and timestamps to a Data frame
        channel_columns = [c for c in self._data.columns if c != "timestamp_unix"]
        df = pd.DataFrame(data=filtered_values, columns=channel_columns)
        df["timestamp_unix"] = self._data["timestamp_unix"]

        # Rearrange columns in the same order as the original data.
        self._data = df[self._data.columns]

    def save_synced_data(self):
        """
        Saves synchronized EEG data to the database. It assumes that the function sync_to_clock
        has been called previously.
        """
        super().save_synced_data()

        df = self._data.reset_index().rename(columns={"index": "id"})
        df["timestamp_iso8601"] = df["timestamp_unix"].apply(
            convert_unix_timestamp_to_iso8601
        )
        df["group_session_id"] = self.group_session
        df["station_id"] = self.station

        df_ekg = df[
            [
                "group_session_id",
                "frequency",
                "station_id",
                "id",
                "timestamp_unix",
                "timestamp_iso8601",
                "aux_ekg",
            ]
        ]

        df_gsr = df[
            [
                "group_session_id",
                "frequency",
                "station_id",
                "id",
                "timestamp_unix",
                "timestamp_iso8601",
                "aux_gsr",
            ]
        ]
        df_eeg = df.drop(columns=["aux_ekg", "aux_gsr"])

        info("Converting DataFrame to records.")
        eeg_records = df_eeg.to_dict("records")
        ekg_records = df_ekg.to_dict("records")
        gsr_records = df_gsr.to_dict("records")

        db = Session(self.db_engine)
        eeg_data = []
        for record in eeg_records:
            eeg_data.append(EEGSync(**record))
        for record in ekg_records:
            eeg_data.append(EKGSync(**record))
        for record in gsr_records:
            eeg_data.append(GSRSync(**record))

        info("Saving to the database.")
        db.add_all(eeg_data)
        db.commit()
        db.close()
