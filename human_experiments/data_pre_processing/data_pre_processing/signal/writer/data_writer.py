from abc import ABC, abstractmethod

import pandas as pd
from sqlalchemy.orm import Session

from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.common.data_source.db import PostgresDB

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

    def write(self, data: pd.DataFrame):
        """
        Writes data to the database external source.

        :param data: data to be written.
        """

        table_class = self.signal_modality.get_data_mode_table_class(self.data_mode)
        db_records = []
        for _, row in data.iterrows():
            db_records.append(table_class(**data.to_dict()))

        with self.db.create_engine() as engine:
            with Session(engine) as db_session:
                db_session.add_all(db_records)
                db_session.commit()
