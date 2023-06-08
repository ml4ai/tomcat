import os
import numpy as np
import pandas as pd
from termcolor import colored
from scipy.signal import butter, lfilter_zi, lfilter


def butter_bandpass(lowcut, highcut, fs, order=5):
    return butter(order, [lowcut, highcut], fs=fs, btype="band")


def butter_bandpass_filter(data, lowcut, highcut, fs, order=5):
    b, a = butter_bandpass(lowcut, highcut, fs, order=order)
    y = lfilter(b, a, data)
    return y


def filter_NIRS(data, full_output_path):
    """
    Use butterworth bandpass filter with highpass set at 0.2hz and
    lowpass set to 0.01hz. Also with a filter order of 3.
    https://doi.org/10.3389%2Ffnhum.2018.00505,
    https://doi.org/10.3390/a11050067,
    https://doi.org/10.3389/fnhum.2019.00331
    """
    print(
        colored("[INFO]", "green", attrs=["bold"]),
        colored("Filtering NIRS data", "green", attrs=["bold"]),
    )
    fs = 10.2
    lowcut = 0.01
    highcut = 0.2

    fb, fa = butter_bandpass(lowcut, highcut, fs)
    zi = lfilter_zi(fb, fa)

    for i in range(40):
        data.iloc[:, i] = butter_bandpass_filter(
            data.iloc[:, i], lowcut, highcut, fs, order=3
        )

    file_path = os.path.join(full_output_path, "NIRS_filtered.csv")
    print(
        colored("[INFO]", "green", attrs=["bold"]),
        colored("Saving filtered NIRS data to", "green", attrs=["bold"]),
        colored(file_path, "green", attrs=["bold"]),
    )
    data.to_csv(file_path, index=False)


def calculate_cv(df_hbo_hbr, df_w1, full_output_path):
    """
    Calculate coeffecient of variance for fNIRS raw siganls, if cv is
    greater than 7.5 then discard then log that channel as
    bad.
    https://doi.org/10.3390/app12010316 refer section 2.4. Channel Exclusion Criterion.
    This function recives raw+HbO data but will use raw W1 to calculate
    the cv.
    """

    channels = {
        "S1-D1",
        "S1-D2",
        "S2-D1",
        "S2-D3",
        "S3-D1",
        "S3-D3",
        "S3-D4",
        "S4-D2",
        "S4-D4",
        "S4-D5",
        "S5-D3",
        "S5-D4",
        "S5-D6",
        "S6-D4",
        "S6-D6",
        "S6-D7",
        "S7-D5",
        "S7-D7",
        "S8-D6",
        "S8-D7",
    }

    print(
        colored("[INFO]", "green", attrs=["bold"]),
        colored("calulating coeffecient of variance of NIRS", "green", attrs=["bold"]),
    )

    rest_state_indices = df_hbo_hbr[df_hbo_hbr["event_type"] == "rest_state"].index

    cv = lambda x: np.std(x, ddof=1) / np.mean(x) * 100
    coef_var = cv(df_w1[rest_state_indices[0] : rest_state_indices[-1]])
    cv_vals = coef_var

    channel_good_or_bad = (coef_var < 7.5).replace(
        {True: "good_channel", False: "bad_channel"}
    )

    df = pd.DataFrame(
        list(zip(channels, cv_vals, channel_good_or_bad)),
        columns=["Channels", "coeff_of_var", "status"],
    )
    file_path = os.path.join(full_output_path, "NIRS_channel_quality.csv")
    print(
        colored("[INFO]", "green", attrs=["bold"]),
        colored(
            "Saving NIRS channel coeffecient of variance data to",
            "green",
            attrs=["bold"],
        ),
        colored(file_path, "green", attrs=["bold"]),
    )
    df.to_csv(file_path, index=False)


def save_NIRS(
    lion_0297_block_NIRS_labeled,
    tiger_0239_block_NIRS_labeled,
    leopard_0171_block_NIRS_labeled,
    lion_0297_raw_w1,
    tiger_0239_raw_w1,
    leopard_0171_raw_w1,
    input_path,
    output_path,
    extract_pkl,
    extract_csv,
    extract_hdf5,
    filter,
):
    # Construct a dictionary to loop through
    df_dict = {
        "lion": [lion_0297_block_NIRS_labeled, lion_0297_raw_w1],
        "tiger": [tiger_0239_block_NIRS_labeled, tiger_0239_raw_w1],
        "leopard": [leopard_0171_block_NIRS_labeled, leopard_0171_raw_w1],
    }

    # Extract the folder name from the input path
    print("Input path: {}".format(input_path))
    folder_name = os.path.basename(input_path)

    for iMac, df in df_dict.items():
        # Create the full output path
        full_output_path = os.path.join(output_path, folder_name, iMac)

        # Ensure the directory exists
        os.makedirs(full_output_path, exist_ok=True)

        if extract_csv:
            # Create the full file path
            file_path = os.path.join(full_output_path, "NIRS.csv")

            # Save the dataframe to a csv file
            print(
                colored("[INFO]", "green", attrs=["bold"]),
                colored("Saving unfiltered NIRS to", "green", attrs=["bold"]),
                colored(file_path, "green", attrs=["bold"]),
            )
            df[0].to_csv(file_path)

        if extract_pkl:
            # Create the full file path
            file_path = os.path.join(full_output_path, "NIRS.pkl")

            # Save the dataframe to a pkl file
            print("Saving NIRS data to: {}".format(file_path))
            print(
                colored("[INFO]", "green", attrs=["bold"]),
                colored("Saving unfiltered NIRS to", "green", attrs=["bold"]),
                colored(file_path, "green", attrs=["bold"]),
            )
            df[0].to_pickle(file_path)

        if extract_hdf5:
            # Create the full file path
            file_path = os.path.join(full_output_path, "NIRS.h5")

            # Save the dataframe to a hdf5 file
            print("Saving NIRS data to: {}".format(file_path))
            print(
                colored("[INFO]", "green", attrs=["bold"]),
                colored("Saving unfiltered NIRS to", "green", attrs=["bold"]),
                colored(file_path, "green", attrs=["bold"]),
            )
            df[0].to_hdf(file_path, key="df", mode="w")

        if filter:
            calculate_cv(df[0], df[1], full_output_path)
            filter_NIRS(df[0], full_output_path)
