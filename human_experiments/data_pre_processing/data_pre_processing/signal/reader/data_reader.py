from abc import ABC, abstractmethod
from typing import List

import pandas as pd
from sqlalchemy import create_engine, Connection, text

from data_pre_processing.signal.entity.modality import Modality


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
                 db_name: str,
                 db_user: str,
                 db_host: str,
                 db_port: str,
                 db_passwd: str):
        """
        Creates an instance of the Postgres data reader.

        :param signal_modality: modality of the signal to be read.
        :param db_name: name of the postgres database.
        :param db_user: user with reading privileges in the database.
        :param db_host: address of the database (e.g., localhost).
        :param db_port: port of the database (e.g., 5432).
        :param db_passwd: password to read from the database.
        """
        super().__init__(signal_modality)
        self.db_name = db_name
        self.db_user = db_user
        self.db_host = db_host
        self.db_port = db_port
        self.db_passwd = db_passwd

    def read_group_sessions(self) -> List[str]:
        """
        Gets a list of group sessions saved in the database.

        :return: a list of group sessions.
        """
        with self._connect() as conn, conn.begin():
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

        channels = ",".join(self.signal_modality.channels)
        query = f"""
            SELECT 
                group_session, 
                station, 
                timestamp_unix,
                {channels}
            FROM {self.signal_modality.table_name} 
            WHERE group_session = '{group_session}'
            ORDER BY station, timestamp_unix
        """

        with self._connect() as conn, conn.begin():
            data = pd.read_sql_query(text(query), conn)
            data["timestamp_unix"] = data["timestamp_unix"].astype(float)

        return data

    def _connect(self) -> Connection:
        """
        Opens a connection with the database.

        :return: a connection object. Make sure to close it properly.
        """
        database_info = f"{self.db_user}:{self.db_name}@{self.db_host}:{self.db_port}"
        connection_string = f"postgresql+psycopg2://{database_info}/{self.db_passwd}"
        return create_engine(connection_string).connect()
