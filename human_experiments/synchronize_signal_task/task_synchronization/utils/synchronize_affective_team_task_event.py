from bisect import bisect_right

import numpy as np
import pandas as pd


def synchronize_affective_team_task_event(signal_df: pd.DataFrame,
                                          task_df: pd.DataFrame) -> tuple[pd.DataFrame, str]:
    """
    Synchronize signal data with task event data
    :param signal_df: signal dataframe
    :param task_df: task dataframe
    :return: signal dataframe with task data synchronized
    """
    # Check time column sorted
    assert task_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"
    assert signal_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"

    output_message = ""

    # Get the 'unix_time' values as a list for binary search
    unix_times = signal_df['timestamp_unix'].tolist()

    # Initialize new columns in the signal data and set as NaN
    signal_df[task_df.columns] = np.nan

    for _, row in task_df.iterrows():
        # Find the index of the closest signal data entry which is before the current task data
        idx = bisect_right(unix_times, row['timestamp_unix']) - 1

        # If there's a next timestamp, and it's closer to the current timestamp
        if idx + 1 < len(unix_times) and \
                abs(unix_times[idx + 1] - row['timestamp_unix']) < abs(unix_times[idx] - row['timestamp_unix']):
            idx += 1  # assign to the next timestamp

        # Assign the task data to this signal data entry
        if idx >= 0:
            for task_data_column in task_df.columns:
                # if the <subject_id>_event_type of this signal data entry is not NaN
                # and the <subject_id>_event_type of this task data entry is not NaN,
                # but they are different, then throw error
                if task_data_column.endswith('_event_type') and \
                        not pd.isna(signal_df.loc[signal_df.index[idx], task_data_column]) and \
                        not pd.isna(row[task_data_column]):
                    if not signal_df.loc[signal_df.index[idx], task_data_column] == \
                           row[task_data_column]:
                        output_message += \
                            f"[WARNING] affective team: Signal data entry {signal_df.index[idx]} " \
                            f"has different {task_data_column} " \
                            f"from task data entry {row['time']}, " \
                            f"proceeding with the overwriting"

                # Assign <subject_id>_event_type to this signal data entry
                # if the <subject_id>_event_type of this task data entry is not NaN
                if task_data_column.endswith('_event_type') and not pd.isna(row[task_data_column]):
                    signal_df.loc[signal_df.index[idx], task_data_column] = row[task_data_column]

                # If the <subject_id>_event_type is intermediate_selection of final_submission,
                # then assign valence and arousal score
                if task_data_column.endswith('_event_type') and \
                        (row[task_data_column] == 'intermediate_selection' or
                         row[task_data_column] == 'final_submission'):
                    signal_df.loc[signal_df.index[idx], 'valence_score'] = row['valence_score']
                    signal_df.loc[signal_df.index[idx], 'arousal_score'] = row['arousal_score']

    return signal_df, output_message
