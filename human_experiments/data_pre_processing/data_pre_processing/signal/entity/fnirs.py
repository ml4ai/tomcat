from typing import List, Type, Union

import pandas as pd

from data_pre_processing.signal.entity.modality import Modality
from data_pre_processing.signal.table.fnirs import FNIRSSync, FNIRSFiltered


class FNIRS(Modality):

    def __init__(self):
        """
        Creates an fNIRS modality.
        """
        super().__init__("fnirs")

    @property
    def channels(self) -> List[str]:
        """
        Gets list of fNIRS channels.

        :return: list of fNIRS channels.
        """
        return CHANNELS

    def get_data_mode_table_class(self, data_mode: str) -> Type[Union[FNIRSSync, FNIRSFiltered]]:
        """
        Gets table class for a specific data mode.

        :param data_mode: one of ["sync", "filtered"], indicating synchronized and filtered data
            tables.
        :return class of the table.
        """

        if data_mode == "sync":
            return FNIRSSync

        if data_mode == "filtered":
            return FNIRSFiltered

        raise ValueError(f"There's no table class for data_mode ({data_mode}).")

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass


CHANNELS = [
    "s1_d1_hbo",
    "s1_d2_hbo",
    "s2_d1_hbo",
    "s2_d3_hbo",
    "s3_d1_hbo",
    "s3_d3_hbo",
    "s3_d4_hbo",
    "s4_d2_hbo",
    "s4_d4_hbo",
    "s4_d5_hbo",
    "s5_d3_hbo",
    "s5_d4_hbo",
    "s5_d6_hbo",
    "s6_d4_hbo",
    "s6_d6_hbo",
    "s6_d7_hbo",
    "s7_d5_hbo",
    "s7_d7_hbo",
    "s8_d6_hbo",
    "s8_d7_hbo",
    "s1_d1_hbr",
    "s1_d2_hbr",
    "s2_d1_hbr",
    "s2_d3_hbr",
    "s3_d1_hbr",
    "s3_d3_hbr",
    "s3_d4_hbr",
    "s4_d2_hbr",
    "s4_d4_hbr",
    "s4_d5_hbr",
    "s5_d3_hbr",
    "s5_d4_hbr",
    "s5_d6_hbr",
    "s6_d4_hbr",
    "s6_d6_hbr",
    "s6_d7_hbr",
    "s7_d5_hbr",
    "s7_d7_hbr",
    "s8_d6_hbr",
    "s8_d7_hbr"
]
