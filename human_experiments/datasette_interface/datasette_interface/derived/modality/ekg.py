from typing import List, Type

import pandas as pd

from data_pre_processing.signal.entity.modality import Modality
# from data_pre_processing.signal.table.eeg import EEGSync, EEGFiltered


class EKG(Modality):

    def __init__(self):
        """
        Creates an EKG modality.
        """
        super().__init__("ekg")

    @property
    def channels(self) -> List[str]:
        """
        Gets list of EKG channels.

        :return: list of EKG channels.
        """
        return CHANNELS

    def get_data_mode_table_class(self, data_mode: str) -> Type[None]:
        """
        Gets table class for a specific data mode.

        :param data_mode: one of ["sync", "filtered"], indicating synchronized and filtered data
            tables.
        :return class of the table.
        """

        # if data_mode == "sync":
        #     return EEGSync
        #
        # if data_mode == "filtered":
        #     return EEGFiltered

        raise ValueError(f"There's no table class for data_mode ({data_mode}).")

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass


CHANNELS = [
    "aux_ekg"
]
