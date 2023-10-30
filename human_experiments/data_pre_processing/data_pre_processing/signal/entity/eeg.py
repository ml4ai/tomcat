from typing import List

import pandas as pd

from data_pre_processing.signal.entity.modality import Modality


class EEG(Modality):

    @property
    def table_name(self) -> str:
        return "eeg_raw"

    @property
    def channels(self) -> List[str]:
        return ["*"]

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass
