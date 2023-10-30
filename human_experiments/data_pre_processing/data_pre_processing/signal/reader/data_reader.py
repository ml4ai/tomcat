from abc import ABC, abstractmethod
from typing import List

import pandas as pd
import psycopg2 as pg

from signal.entity.modality import Modality


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
    def get_group_sessions(self) -> List[str]:
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
                 db_port: str):
        """
        Creates an instance of the Postgres data reader.

        :param signal_modality: modality of the signal to be read.
        :param db_name: name of the postgres database.
        :param db_user: user with reading privileges in the database.
        :param db_host: address of the database (e.g., localhost).
        :param db_port: port of the database (e.g., 5432).
        """
        super().__init__(signal_modality)
        self.db_name = db_name
        self.db_user = db_user
        self.db_host = db_host
        self.db_port = db_port

    def get_group_sessions(self) -> List[str]:
        """
        Gets a list of group sessions saved in the database.

        :return: a list of group sessions.
        """
        with pg.connect(user=self.db_user,
                        host=self.db_host,
                        port=self.db_port,
                        database=self.db_name) as conn:
            with conn.cursor() as cursor:
                query = "SELECT * FROM group_session"
                cursor.execute(query)
                rows = cursor.fetchall()

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
                {channels},
            FROM {self.signal_modality.table_name} 
            WHERE group_session = '{group_session}'
            ORDER BY station, timestamp_unix
        """

        with pg.connect(user=self.db_user,
                        host=self.db_host,
                        port=self.db_port,
                        database=self.db_name) as conn:
            data = pd.read_sql_query(query, conn)

        return data
