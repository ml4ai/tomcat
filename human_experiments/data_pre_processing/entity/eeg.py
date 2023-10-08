from typing import List

from entity.signal import SignalModality

import pandas as pd


class EEG(SignalModality):

    @property
    def table_name(self) -> str:
        return "eeg_raw"

    @property
    def channels(self) -> List[str]:
        return ["*"]

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass
