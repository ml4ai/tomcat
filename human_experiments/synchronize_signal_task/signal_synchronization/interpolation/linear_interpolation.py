import numpy as np
import pandas as pd

from ..utils import generate_time_series_end_time


def linear_interpolation(df: pd.DataFrame,
                         frequency: float,
                         start_time: float | None = None) -> pd.DataFrame:
    """
    Interpolate DataFrame to given frequency
    :param df: signal DataFrame
    :param frequency: sampling frequency in Hz
    :param start_time: start time in seconds
    :return: interpolated DataFrame
    """
    # Check timestamp_unix column sorted
    assert df['timestamp_unix'].is_monotonic_increasing, "timestamp_unix column must be sorted"

    # Generate new time series
    if start_time is None:
        start_time = df['timestamp_unix'].min()
    else:
        assert start_time >= df['timestamp_unix'].min(), "start_time must be >= df['timestamp_unix'].min()"
        assert start_time <= df['timestamp_unix'].max(), "start_time must be <= df['timestamp_unix'].max()"

    end_time = df['timestamp_unix'].max()
    interp_time_series = generate_time_series_end_time(start_time, end_time, frequency)

    # Interpolate signals
    df_new = df.drop(columns=["timestamp_unix"]).apply(
        lambda col: np.interp(interp_time_series, df['timestamp_unix'], col)
    )
    df_new['timestamp_unix'] = interp_time_series

    return df_new
