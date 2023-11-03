from abc import ABC, abstractmethod

import pandas as pd
from sqlalchemy.orm import Session

from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.common.data_source.db import PostgresDB
from sqlalchemy import insert

# Writing to raw data tables is not allowed with this writer class..
DATA_MODES = ["sync", "filtered"]


class DataWriter(ABC):
    """
    This class handles data writing from different modalities.
    """

    def __init__(self, signal_modality: Modality, data_mode: str):
        """
        Creates a writer instance.

        :param signal_modality: modality of the signal to be written.
        :param data_mode: one of ["sync", "filtered"], indicating whether we wish to write to
            synchronized or filtered data tables.
        """

        if data_mode not in DATA_MODES:
            raise ValueError(f"Invalid data_mode ({data_mode}). It must be one of {DATA_MODES}.")

        self.signal_modality = signal_modality
        self.data_mode = data_mode

    @abstractmethod
    def write(self, data: pd.DataFrame):
        """
        Writes data to an external source.

        :param data: data to be written.
        """
        pass


class PostgresDataWriter(DataWriter):
    """
    This class handles data writing to a Postgres database.
    """

    def __init__(self,
                 signal_modality: Modality,
                 data_mode: str,
                 db: PostgresDB):
        """
        Creates an instance of the Postgres data reader.

        :param signal_modality: modality of the signal to be read.
        :param data_mode: one of ["sync", "filtered"], indicating whether we wish to write to
            synchronized or filtered data tables.
        :param db: db object to handle operations on a Postgres cluster.
        """
        super().__init__(signal_modality, data_mode)

        self.db = db

        self._create_table()

    def _create_table(self):
        table_class = self.signal_modality.get_data_mode_table_class(self.data_mode)
        table_class.__table__.create(self.db.create_engine(), checkfirst=True)

    def write(self, data: pd.DataFrame):
        """
        Writes data to the database external source.

        :param data: data to be written.
        """
        # db_records = []
        table_class = self.signal_modality.get_data_mode_table_class(self.data_mode)
        # for _, row in data.iterrows():
        #     # record = table_class(**row.to_dict())
        #     # db_records.append(record)
        #     db_records.append(row.to_dict())

        db_records = data.to_dict("records")

        print(f"Saving {len(db_records)} records to the database")
        with Session(self.db.create_engine()) as db_session:
            db_session.execute(
                insert(table_class),
                db_records
            )
            # db_session.add_all(db_records)
            db_session.commit()
        # with self.db.create_engine().connect() as conn:
        #     data.to_sql(name=table_class.__table__, con=conn, if_exists='append')
