import pandas as pd


def remove_columns(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    # get columns_to_drop in both dataframe and columns
    columns_to_drop = [col for col in columns if col in df.columns]

    return df.drop(columns=columns_to_drop)
