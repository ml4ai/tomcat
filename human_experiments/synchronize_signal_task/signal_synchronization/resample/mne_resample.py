import pandas as pd

from mne.filter import resample


def mne_resample(signal_df: pd.DataFrame,
                 src_frequency: float,
                 new_frequency: float) -> pd.DataFrame:
    """
    Resample DataFrame to given frequency using MNE
    :param signal_df: Dataframe with signal
    :param src_frequency: frequency of the signal Dataframe in Hz
    :param new_frequency: desired new frequency in Hz for resampling
    :return: resampled signal DataFrame
    """
    sample_scale = new_frequency / src_frequency

    # Interpolate signals
    signal_resampled_df = signal_df.apply(
        lambda col: resample(col.to_numpy(), sample_scale, npad='auto')
    )

    return signal_resampled_df
