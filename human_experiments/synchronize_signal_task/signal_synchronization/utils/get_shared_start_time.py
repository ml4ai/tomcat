import pandas as pd

from .is_time_overlapping import is_time_overlapping


def get_shared_start_time(dfs: list[pd.DataFrame]) -> float:
    """
    Get shared start and end time from list of DataFrames
    :param dfs: list of DataFrames
    :return: shared start and end time
    """
    assert len(dfs) > 0, "List of DataFrames is empty"
    assert is_time_overlapping(dfs), "DataFrames are not overlapping"

    start_time = max(df['timestamp_unix'].min() for df in dfs)
    return start_time
