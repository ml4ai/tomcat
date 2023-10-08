import argparse
from functools import partial
from logging import info
import math
from typing import List

from mne.filter import resample
import numpy as np
from tqdm import tqdm

from config import NUM_PROCESSES
from reader.data_reader import DataReader

from multiprocessing import Pool


def sync_raw_signals_single_job(group_sessions: List[str],
                                data_reader: DataReader,
                                source_frequency: int,
                                target_frequency: int):
    for group_session in group_sessions:
        # TODO add message to queue
        data = data_reader.read(group_session)

        start_time = data_reader.get_overlapping_window(data)[0]

        for station in data["station"].unique():
            station_data = data[data["station"] == station]
            original_values = station_data[data_reader.signal_modality.channels].values

            upsampled_values = resample(x=original_values, up=target_frequency / source_frequency, npad="auto", axis=0)

            interpolated_signal = linear_interpolation(
                upsampled_values, target_frequency, start_time
            )



        # TODO: start_time = float(math.ceil(get_shared_start_time(data)))
        # Is ceiling really necessary here?
        start_time = data_reader.get_overlapping_window(data)[0]


def sync_raw_signals_in_parallel(data_reader: DataReader, source_frequency: int, target_frequency: int, num_jobs: int):
    job_fn = partial(sync_raw_signals_single_job,
                     data_reader=data_reader,
                     source_frequency=source_frequency,
                     target_frequency=target_frequency)
    group_sessions = data_reader.get_group_sessions()

    with Pool(processes=num_jobs) as pool:
        list(tqdm(pool.imap(job_fn, group_sessions), total=len(group_sessions)))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
            Synchronizes raw signals from a specific modality across participants in a specific frequency. We first,
            resample (typically upsample) the signals and interpolate to generate equally spaced values, which we 
            can then align across participants.           
        """
    )

    parser.add_argument("--modality", type=str, required=True, choices=["eeg", "ekg", "gsr", "fnirs", "gaze"],
                        help="Modality of the raw data.")
    parser.add_argument("--freq", type=int, required=True,
                        help="Desired frequency. Typically higher than the empirical frequency of the raw data.")
    parser.add_argument("--n_jobs", type=int, required=False, default=NUM_PROCESSES,
                        help="Number of jobs for parallel processing.")

    args = parser.parse_args()

    # TODO - Configure log

    sync_raw_signals(args.modality, args.freq, args.n_jobs)
