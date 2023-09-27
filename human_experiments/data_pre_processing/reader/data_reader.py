from typing import List

import pandas as pd
import psycopg2 as pg

from entity.modality import Modality


class DataReader:

    def __init__(self, modality: Modality):
        self.modality = modality

    def get_group_sessions(self) -> List[str]:
        """
        Returns a list of group sessions from a source (e.g. database)
        """
        raise NotImplementedError


class PostgresDataReader(DataReader):

    def __init__(self, modality: Modality, db_name: str, db_user: str, db_host: str, db_port: str):
        super().__init__(modality)
        self.db_name = db_name
        self.db_user = db_user
        self.db_host = db_host
        self.db_port = db_port

    def get_group_sessions(self) -> List[str]:
        with pg.connect(user=self.db_user, host=self.db_host, port=self.db_port, database=self.db_name) as conn:
            with conn.cursor() as cursor:
                query = "SELECT * FROM group_session"
                cursor.execute(query)
                rows = cursor.fetchall()

        return [r[0] for r in rows]

    def read(self, group_session: str) -> pd.DataFrame:
        # TODO: Complete with other modalities
        channels = ",".join(self.modality.channels)
        query = f"""
            SELECT 
                group_session, 
                station, 
                timestamp_unix,
                {channels},
            FROM {self.modality.table_name} 
            WHERE group_session = {group_session} 
            ORDER BY station, timestamp_unix
        """

        with pg.connect(user=self.db_user, host=self.db_host, port=self.db_port, database=self.db_name) as conn:
            data = pd.read_sql_query(query, conn)

        return data
