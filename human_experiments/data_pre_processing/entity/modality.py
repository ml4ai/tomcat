from typing import List

import pandas as pd


class Modality:

    def synchronize(self, data: pd.DataFrame) -> pd.DataFrame:
        # TODO: implement synchronization
        pass

    def filter(self, data: pd.DataFrame) -> pd.DataFrame:
        # It is specific to each modality
        raise NotImplementedError

    @property
    def table_name(self) -> str:
        raise NotImplementedError

    @property
    def channels(self) -> List[str]:
        raise NotImplementedError
