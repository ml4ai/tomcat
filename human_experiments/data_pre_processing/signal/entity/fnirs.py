from typing import List

from signal.entity.modality import Modality

import pandas as pd


class FNIRS(Modality):

    @property
    def table_name(self) -> str:
        return "fnirs_raw"

    @property
    def channels(self) -> List[str]:
        return ["*"]

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass
