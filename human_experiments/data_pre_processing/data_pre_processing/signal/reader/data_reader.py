from abc import ABC, abstractmethod
from typing import List

import pandas as pd
from sqlalchemy import text

from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.common.data_source.db import PostgresDB

DATA_MODES = ["raw", "sync", "filtered"]


class DataReader(ABC):
    """
    This class handles data reading from different modalities.
    """

    def __init__(self, signal_modality: Modality, data_mode: str):
        """
        Creates a reader instance.

        :param signal_modality: modality of the signal to be read.
        :param data_mode: one of ["raw", "sync", "filtered"], indicating whether we wish to read
            raw, synchronized or filtered data.
        """

        if data_mode not in DATA_MODES:
            raise ValueError(f"Invalid data_mode ({data_mode}). It must be one of {DATA_MODES}.")

        self.signal_modality = signal_modality
        self.data_mode = data_mode

    @abstractmethod
    def read_group_sessions(self) -> List[str]:
        """
        Returns a list of group sessions from a source (e.g. database).

        :return list of group sessions.
        """
        pass

    @abstractmethod
    def read(self, group_session: str) -> pd.DataFrame:
        """
        Returns data signals from a group session.

        :return signals (a time series of data values over time).
        """
        pass


class PostgresDataReader(DataReader):
    """
    This class handles data loading from a Postgres database.
    """

    def __init__(self,
                 signal_modality: Modality,
                 data_mode: str,
                 db: PostgresDB):
        """
        Creates an instance of the Postgres data reader.

        :param signal_modality: modality of the signal to be read.
        :param data_mode: one of ["raw", "sync", "filtered"], indicating whether we wish to read
            raw, synchronized or filtered data.
        :param db: db object to handle operations on a Postgres cluster.
        """
        super().__init__(signal_modality, data_mode)

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

    def read(self, group_session: str) -> pd.DataFrame:
        """
        Reads data signals from a group session from a modality-specific table.

        :param group_session: group session in which the data was recorded.
        :return signals (a time series of data values over time).
        """

        if self.data_mode == "raw" and self.signal_modality.name in ("ekg", "gsr"):
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
            FROM {modality_name}_{self.data_mode} 
            WHERE group_session = '{group_session}'
            ORDER BY station, timestamp_unix
        """

        with self.db.create_engine().connect() as conn, conn.begin():
            data = pd.read_sql_query(text(query), conn)
            data["timestamp_unix"] = data["timestamp_unix"].astype(float)

        return data
