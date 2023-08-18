import pandas as pd


def is_time_overlapping(dfs: list[pd.DataFrame]) -> bool:
    """
    Check if all DataFrames have overlapping time ranges among each other
    :param dfs: list of DataFrames
    :return: True if all DataFrames have overlapping time ranges among each other, False otherwise
    """
    # Ensure all DataFrames are not empty
    assert all(len(df) > 0 for df in dfs), "All DataFrames must be non-empty"

    # Ensure all DataFrames have unix_time column
    assert all('timestamp_unix' in df.columns for df in dfs), "All DataFrames must have unix_time column"

    return all(df['timestamp_unix'].min() <= dfs[0]['timestamp_unix'].max() for df in dfs) \
        and all(df['timestamp_unix'].max() >= dfs[0]['timestamp_unix'].min() for df in dfs)
