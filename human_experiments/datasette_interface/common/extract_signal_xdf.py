import pandas as pd
import numpy as np


def _get_channels(xdf_stream: dict) -> list[str]:
    channels = []
    for channel in xdf_stream["info"]["desc"][0]["channels"][0]["channel"]:
        channels.append(channel["label"][0])

    return channels


def _get_signal(xdf_stream: dict) -> np.ndarray:
    return xdf_stream["time_series"]


def _get_timestamps(xdf_stream: dict) -> np.ndarray:
    return xdf_stream["time_stamps"]


def extract_signal_xdf(xdf_stream: dict,
                       remove_channels: list[str] | None = None,
                       desired_channels: list[str] | None = None,
                       unit_conversion: float | None = None) -> pd.DataFrame:
    # Get channels, signal and timestamps from XDF stream
    channels = _get_channels(xdf_stream)
    signal = _get_signal(xdf_stream)
    timestamps = _get_timestamps(xdf_stream)

    # Creating a DataFrame for the signal
    df_signal = pd.DataFrame(signal, columns=channels)

    # Removing channels
    if remove_channels:
        removing_channels = list(set(df_signal.columns) & set(remove_channels))
        df_signal = df_signal.drop(columns=removing_channels)

    # Convert units except timestamps
    if unit_conversion is not None:
        df_signal = df_signal * unit_conversion

    if desired_channels:
        # Append channels with None values if they are not in the DataFrame
        for channel in desired_channels:
            if channel not in df_signal.columns:
                df_signal[channel] = np.nan

        # Reorder columns
        df_signal = df_signal[desired_channels]

    # Create a DataFrame with timestamps
    df_timestamps = pd.DataFrame(timestamps, columns=['timestamp_unix'])
    df = pd.concat([df_timestamps, df_signal], axis=1)

    return df
