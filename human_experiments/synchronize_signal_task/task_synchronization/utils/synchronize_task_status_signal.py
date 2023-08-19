import pandas as pd


def synchronize_task_status_signal(signal_df: pd.DataFrame, task_df: pd.DataFrame) -> pd.DataFrame:
    """
    Group signals relevant to a task
    :param signal_df: signal dataframe
    :param task_df: task dataframe
    :return: signal dataframe with signals relevant to the task
    """
    # Check time column sorted
    assert task_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"
    assert signal_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"

    start_time = task_df["timestamp_unix"].min()
    end_time = task_df["timestamp_unix"].max()

    # Get the index of the element just before start_time
    start_index = signal_df["timestamp_unix"].searchsorted(start_time, side='left') - 1

    # Get the index of the element just after end_time
    end_index = signal_df["timestamp_unix"].searchsorted(end_time, side='right')

    # Check and adjust if indices go out of bounds
    start_index = max(start_index, 0)
    end_index = min(end_index, len(signal_df) - 1)

    # Filter signal_df
    signal_df = signal_df.iloc[start_index:end_index + 1]

    # Drop the columns with any NaN value
    signal_df = signal_df.dropna(axis=1, how='any')

    return signal_df
