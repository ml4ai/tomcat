from bisect import bisect_right

import numpy as np
import pandas as pd


def synchronize_task_event_signal(signal_df: pd.DataFrame,
                                  task_df: pd.DataFrame) -> pd.DataFrame:
    """
    Synchronize signal data with task event data
    :param signal_df: signal dataframe
    :param task_df: task dataframe
    :return: signal dataframe with task data synchronized
    """
    # Check time column sorted
    assert task_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"
    assert signal_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"

    # Get the 'unix_time' values as a list for binary search
    unix_times = signal_df['timestamp_unix'].tolist()

    task_df.rename(columns={'timestamp_unix': 'task_timestamp_unix'}, inplace=True)

    # Initialize new columns in the signal data and set as NaN
    signal_df[task_df.columns] = np.nan

    for _, row in task_df.iterrows():
        # Find the index of the closest signal data entry which is before the current task data
        idx = bisect_right(unix_times, row['task_timestamp_unix']) - 1

        # If there's a next timestamp, and it's closer to the current timestamp
        if idx + 1 < len(unix_times) and \
                abs(unix_times[idx + 1] - row['task_timestamp_unix']) < abs(unix_times[idx] - row['task_timestamp_unix']):
            idx += 1  # assign to the next timestamp

        if idx >= 0:
            # Filter the columns with the "_event_type" suffix
            row_with_suffix = signal_df.loc[signal_df.index[idx]].filter(like='_event_type')

            # Check if any columns were found with the "_event_type" suffix
            if not row_with_suffix.empty and (row_with_suffix == "final_submission").any():
                continue  # Skip this iteration if "final_submission" is found in any of the "_event_type" columns

            # Assign the task data to this signal data entry
            signal_df.loc[signal_df.index[idx], task_df.columns] = row

    signal_df = signal_df.drop(columns=['task_timestamp_unix'])

    return signal_df
