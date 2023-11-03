from typing import List, Type, Union

import pandas as pd

from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.signal.table.eeg import EEGSync, EEGFiltered


class EEG(Modality):

    def __init__(self):
        """
        Creates an EEG modality.
        """
        super().__init__("eeg")

    @property
    def channels(self) -> List[str]:
        """
        Gets list of EEG channels.

        :return: list of EEG channels.
        """
        return CHANNELS

    def get_data_mode_table_class(self, data_mode: str) -> Type[Union[EEGSync, EEGFiltered]]:
        """
        Gets table class for a specific data mode.

        :param data_mode: one of ["sync", "filtered"], indicating synchronized and filtered data
            tables.
        :return class of the table.
        """

        if data_mode == "sync":
            return EEGSync

        if data_mode == "filtered":
            return EEGFiltered

        raise ValueError(f"There's no table class for data_mode ({data_mode}).")

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass


CHANNELS = [
    "aff1h",
    "f7",
    "fc5",
    "c3",
    "t7",
    "tp9",
    "pz",
    "p3",
    "p7",
    "o1",
    "o2",
    "p8",
    "p4",
    "tp10",
    "cz",
    "c4",
    "t8",
    "fc6",
    "fcz",
    "f8",
    "aff2h"
]
