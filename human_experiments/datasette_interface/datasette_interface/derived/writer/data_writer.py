from abc import ABC, abstractmethod
from typing import Type

import pandas as pd
from sqlalchemy.orm import Session
from sqlalchemy import insert
import logging
import time

from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.common.data_source.db import PostgresDB
from data_pre_processing.signal.table.base import Base


BATCH_SIZE = 5000


class DataWriter(ABC):
    """
    This class handles data writing from different modalities.
    """

    def __init__(self, signal_modality: Modality):
        """
        Creates a writer instance.

        :param signal_modality: modality of the signal to be written.
        """

        self.signal_modality = signal_modality

    @abstractmethod
    def write_unfiltered(self, data: pd.DataFrame):
        """
        Writes synchronized unfiltered data to the database external source.

        :param data: data to be written.
        """
        pass

    @abstractmethod
    def write_filtered(self, data: pd.DataFrame):
        """
        Writes synchronized filtered data to the database external source.

        :param data: data to be written.
        """
        pass


class PostgresDataWriter(DataWriter):
    """
    This class handles data writing to a Postgres database.
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

    def write_unfiltered(self, data: pd.DataFrame):
        """
        Writes synchronized unfiltered data to the database external source.

        :param data: data to be written.
        """

        self._write(data, self.signal_modality.get_data_mode_table_class("unfiltered"))

    def write_filtered(self, data: pd.DataFrame):
        """
        Writes synchronized filtered data to the database external source.

        :param data: data to be written.
        """

        self._write(data, self.signal_modality.get_data_mode_table_class("filtered"))

    def _write(self, data: pd.DataFrame, table_class: Type[Base]):
        """
        Writes synchronized data to the database external source.

        :param data: data to be written.
        :param table_class: ORM representation of the table where data must be persisted to.
        """

        logger = logging.getLogger()

        logger.info("Records to dict.")
        db_records = data.to_dict("records")

        # Saving in batches is a bit faster than calling execute once for all records.
        start_time = time.time()
        batches = [db_records[i:i + BATCH_SIZE] for i in range(0, len(db_records), BATCH_SIZE)]

        logger.info(f"Saving {len(db_records)} records to the database in {len(batches)}.")
        with Session(self.db.create_engine()) as db_session:
            for i, batch in enumerate(batches):
                logger.info(f"Inserting batch {i + 1} out of {len(batches)}.")
                db_session.execute(
                    insert(table_class),
                    batch
                )
            db_session.commit()
        self.exec_batch = False
        logger.info(f"---------------> {time.time() - start_time} seconds")

