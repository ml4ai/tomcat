import pandas as pd
from .is_valid_signal import is_valid_signal


def remove_invalid_columns(signal_df: pd.DataFrame,
                           session: str,
                           task_name: str,
                           modality: str) -> pd.DataFrame:
    # Remove invalid signals in a task
    clean_df = signal_df
    for station in ["lion", "tiger", "leopard"]:
        if not is_valid_signal(session, station, task_name, modality):
            clean_df = clean_df.drop(
                columns=[col for col in clean_df.columns if col.startswith(f"{station}_{modality}")]
            )

    # Drop the columns with any NaN value
    clean_df = clean_df.dropna(axis=1, how='any')

    return clean_df
