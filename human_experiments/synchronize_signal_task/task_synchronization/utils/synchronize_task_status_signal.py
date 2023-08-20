import pandas as pd


def synchronize_task_status_signal(signal_df: pd.DataFrame,
                                   task_df: pd.DataFrame) -> pd.DataFrame:
    """
    Synchronize task status with signals
    :param signal_df: signal dataframe
    :param task_df: task dataframe
    :return: synchronized dataframe
    """
    # Check time column sorted
    assert task_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"
    assert signal_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"

    task_df.rename(columns={'timestamp_unix': 'task_timestamp_unix'}, inplace=True)

    # Assigning the events to the closest signal entries
    merged_df = pd.merge_asof(signal_df,
                              task_df,
                              left_on='timestamp_unix',
                              right_on='task_timestamp_unix',
                              direction='nearest')

    merged_df = merged_df.drop(columns=['task_timestamp_unix'])

    return merged_df
