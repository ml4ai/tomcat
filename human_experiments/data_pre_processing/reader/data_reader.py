from typing import List, Tuple

import numpy as np
import pandas as pd
import psycopg2 as pg

from entity.signal import SignalModality


class DataReader:

    def __init__(self, signal_modality: SignalModality):
        self.signal_modality = signal_modality

    def get_group_sessions(self) -> List[str]:
        """
        Returns a list of group sessions from a source (e.g. database)
        """
        raise NotImplementedError

    def read(self, group_session: str) -> pd.DataFrame:
        """
        Returns a pandas DataFrame with a list of signals from a group session
        """
        raise NotImplementedError

    @staticmethod
    def get_overlapping_window(data: pd.DataFrame) -> Tuple[float, float]:
        global_interval = [0, 0]
        intervals = []
        for station in data["station"].unique():
            start_time = data[data["station"] == station]["timestamp_unix"].min()
            end_time = data[data["station"] == station]["timestamp_unix"].max()
            intervals.append((start_time, end_time))

            global_interval[0] = np.maximum(global_interval, start_time)
            global_interval[1] = np.minimum(global_interval, end_time)

        # Check if all intervals overlap
        for i in range(len(intervals)):
            for j in range(i+1, len(intervals)):
                overlap = intervals[j][0] <= intervals[i][1] and intervals[j][1] >= intervals[i][0]
                if not overlap:
                    raise Exception("Signals from different stations do not overlap in time.")

        return tuple(global_interval)


class PostgresDataReader(DataReader):

    def __init__(self, signal_modality: SignalModality, db_name: str, db_user: str, db_host: str, db_port: str):
        super().__init__(signal_modality)
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

        with pg.connect(user=self.db_user, host=self.db_host, port=self.db_port, database=self.db_name) as conn:
            data = pd.read_sql_query(query, conn)

        return data
