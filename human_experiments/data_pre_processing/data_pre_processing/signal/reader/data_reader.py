from abc import ABC, abstractmethod
from typing import List

import pandas as pd
from sqlalchemy import text

from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.common.data_source.db import PostgresDB


class DataReader(ABC):
    """
    This class handles data reading from different modalities.
    """

    def __init__(self, signal_modality: Modality):
        """
        Creates a reader instance.

        :param signal_modality: modality of the signal to be read.
        """

        self.signal_modality = signal_modality

    @abstractmethod
    def read_group_sessions(self) -> List[str]:
        """
        Returns a list of group sessions from a source (e.g. database).

        :return list of group sessions.
        """
        pass

    @abstractmethod
    def read_raw(self, group_session: str) -> pd.DataFrame:
        """
        Returns raw data signals from a group session.

        :return signals (a time series of data values over time).
        """
        pass

    @abstractmethod
    def sync_data_exists(self, group_session: str, station: str,
                                  frequency: int) -> bool:
        """
        Checks whether there's sync and filtered signals saved for a specific group session,
        station and frequency.

        :param group_session: group session
        :param station: station
        :param frequency: frequency of the sync signal.
        :return True if there's saved data.
        """
        pass


class PostgresDataReader(DataReader):
    """
    This class handles data loading from a Postgres database.
    """

    def __init__(self,
                 signal_modality: Modality,
                 db: PostgresDB):
        """
        Creates an instance of the Postgres data reader.

        :param signal_modality: modality of the signal to be read.
        :param db: db object to handle operations on a Postgres cluster.
        """
        super().__init__(signal_modality)

        self.db = db

    def read_group_sessions(self) -> List[str]:
        """
        Gets a list of group sessions saved in the database.

        :return: a list of group sessions.
        """
        with self.db.create_engine().connect() as conn, conn.begin():
            query = "SELECT * FROM group_session ORDER BY group_session"
            res = conn.execute(text(query))
            rows = res.fetchall()

        return [r[0] for r in rows]

    def read_raw(self, group_session: str) -> pd.DataFrame:
        """
        Reads data signals from a group session from a modality-specific table.

        :param group_session: group session in which the data was recorded.
        :return signals (a time series of data values over time).
        """

        if self.signal_modality.name in ("ekg", "gsr"):
            # Raw GSR and EKG values are in the EEG table.
            modality_name = "eeg"
        else:
            modality_name = self.signal_modality.name

        channels_str = ",".join(self.signal_modality.channels)
        query = f"""
            SELECT 
                group_session,
                station,
                timestamp_unix,
                {channels_str}
            FROM {modality_name}_raw
            WHERE group_session = '{group_session}'
            ORDER BY station, timestamp_unix
        """

        with self.db.create_engine().connect() as conn, conn.begin():
            data = pd.read_sql_query(text(query), conn)
            data["timestamp_unix"] = data["timestamp_unix"].astype(float)

        return data

    def sync_data_exists(self, group_session: str, station: str, frequency: int) -> bool:
        """
        Checks whether there's sync and filtered signals saved for a specific group session,
        station and frequency.

        :param group_session: group session
        :param station: station
        :param frequency: frequency of the sync signal.
        :return True if there's saved data.
        """

        query = f"""
                    SELECT 
                        COUNT(*)
                    FROM {self.signal_modality.name}_sync_filtered
                    WHERE group_session = '{group_session}'
                      AND frequency = {frequency}
                      AND station = '{station}'
                """

        with self.db.create_engine().connect() as conn, conn.begin():
            res = conn.execute(text(query))
            return res.fetchone()[0] > 0
