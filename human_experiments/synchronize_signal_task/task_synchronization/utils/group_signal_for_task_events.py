import bisect

import numpy as np
import pandas as pd


def _check_event_signal_assignments(signal_times, event_times):
    # Create a list to track assignments
    assignments = [False] * len(signal_times)

    for event_time in event_times:
        # Find the position where this event time would be inserted in sorted_signals
        position = bisect.bisect(signal_times, event_time)

        # Make sure the event time doesn't fall after all signal times
        if position == len(signal_times):
            position -= 1

        # Make sure the event time doesn't fall before all signal times
        if position > 0 and abs(signal_times[position - 1] - event_time) <= abs(
                signal_times[position] - event_time):
            position -= 1

        # Assert that the signal at this position hasn't been assigned yet
        assert not assignments[position], \
            f"Signal at position {position} and time {signal_times[position]} already assigned"

        # Assign this signal
        assignments[position] = True


def group_signal_for_task_event(signal_df: pd.DataFrame,
                                task_df: pd.DataFrame,
                                check_event_assignments: bool = True) -> pd.DataFrame:
    """
    Group signals relevant to a task
    :param signal_df: signal dataframe
    :param task_df: task dataframe
    :param check_event_assignments: whether to check if all events have been assigned to a signal
    :return: signal dataframe with signals relevant to the task
    """
    # Check time column sorted
    assert task_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"
    assert signal_df["timestamp_unix"].is_monotonic_increasing, "timestamp_unix column must be sorted"

    # Get task start and end time
    start_time = task_df["timestamp_unix"].min()
    end_time = task_df["timestamp_unix"].max()

    # Get the index of the element closest to start_time
    start_index = bisect.bisect_left(signal_df["timestamp_unix"].values, start_time)
    # Check if the index is not at the end of the array and if the next value is closer
    if start_index != len(signal_df) and \
            np.abs(signal_df.iloc[start_index]["timestamp_unix"] - start_time) > \
            np.abs(signal_df.iloc[start_index + 1]["timestamp_unix"] - start_time):
        start_index += 1

    # Get the index of the element closest to end_time
    end_index = bisect.bisect_right(signal_df["timestamp_unix"].values, end_time)
    # Check if the index is not at the start of the array and if the previous value is closer
    if end_index != 0 and end_index < len(signal_df) and \
            np.abs(signal_df.iloc[end_index - 1]["timestamp_unix"] - end_time) <= \
            np.abs(signal_df.iloc[end_index]["timestamp_unix"] - end_time):
        end_index -= 1

    # Filter signal_df
    signal_df = signal_df.iloc[start_index:end_index + 1]

    # Check that all event times are assigned to a signal
    if check_event_assignments:
        signal_times = signal_df["timestamp_unix"].values
        event_times = task_df["timestamp_unix"].values
        _check_event_signal_assignments(signal_times, event_times)

    # Drop the columns with any NaN value
    signal_df = signal_df.dropna(axis=1, how='any')

    return signal_df
