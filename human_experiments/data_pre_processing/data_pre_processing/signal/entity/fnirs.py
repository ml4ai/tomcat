from typing import List

from signal.entity.modality import Modality

import pandas as pd


class FNIRS(Modality):

    @property
    def table_name(self) -> str:
        return "fnirs_raw"

    @property
    def channels(self) -> List[str]:
        return FNIRS_CHANNELS

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass


FNIRS_CHANNELS = [
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
