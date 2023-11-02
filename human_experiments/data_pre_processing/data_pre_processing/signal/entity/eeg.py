from typing import List

import pandas as pd

from data_pre_processing.signal.entity.modality import Modality


class EEG(Modality):

    def __init__(self):
        """
        Creates an EEG modality.
        """
        super().__init__("eeg")

    @property
    def channels(self) -> List[str]:
        return ["aff1h", "aff5h"]

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: Implement signal filtering
        pass
