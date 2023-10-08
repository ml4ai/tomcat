from typing import List

from entity.signal import SignalModality

import pandas as pd


class FNIRS(SignalModality):

    @property
    def table_name(self) -> str:
        return "fnirs_raw"

    @property
    def channels(self) -> List[str]:
        return ["*"]

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass
