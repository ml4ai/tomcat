import argparse
from functools import partial
import logging
from typing import List
import os
import sys


from tqdm import tqdm

from data_pre_processing.common.config import NUM_PROCESSES, LOG_DIR, OUT_DIR, USER
from data_pre_processing.signal.reader.data_reader import DataReader, PostgresDataReader
from data_pre_processing.signal.common.constants import MODALITIES

from multiprocessing import Pool


def sync_raw_signals_in_parallel(data_reader: DataReader, source_frequency: int,
                                 target_frequency: int, num_jobs: int):
    job_fn = partial(sync_raw_signals_single_job,
                     data_reader=data_reader,
                     source_frequency=source_frequency,
                     target_frequency=target_frequency)

    group_sessions = data_reader.read_group_sessions()
    with Pool(processes=num_jobs) as pool:
        list(tqdm(pool.imap(job_fn, group_sessions), total=len(group_sessions)))


def sync_raw_signals_single_job(group_sessions: List[str],
                                data_reader: DataReader,
                                source_frequency: int,
                                target_frequency: int):
    for group_session in group_sessions:
        # TODO add message to queue
        data = data_reader.read(group_session)

        start_time, end_time = get_overlapping_window_across_stations(data)

        for station in data["station"].unique():
            station_data = data[data["station"] == station]
            raw_values = station_data[data_reader.signal_modality.channels].values

            upsampled_values = data_reader.signal_modality.upsample(
                data=raw_values,
                source_frequency=source_frequency,
                target_frequency=target_frequency
            )

            data_reader.signal_modality.interpolate(
                data=upsampled_values,
                source_frequency=source_frequency,
                target_frequency=target_frequency
            )

            upsampled_values = resample(x=original_values,
                                        up=target_frequency / source_frequency,
                                        npad="auto",
                                        axis=0)

            interpolated_signal = linear_interpolation(
                upsampled_values, target_frequency, start_time
            )

        # TODO: start_time = float(math.ceil(get_shared_start_time(data)))
        # Is ceiling really necessary here?
        start_time = data_reader.get_overlapping_window(data)[0]


def get_overlapping_window_across_stations(data: pd.DataFrame) -> Tuple[float, float]:
    """
    Returns a tuple containing overlapping start time and end time across data signals from
    different stations.

    :param data: data signals.
    :return: a tuple containing the latest initial time and earliest end time of data signals
    across different stations.
    """

    global_interval = [0, 0]
    intervals = []
    for station in data["station"].unique():
        start_time = data[data["station"] == station]["timestamp_unix"].min()
        end_time = data[data["station"] == station]["timestamp_unix"].max()
        intervals.append((start_time, end_time))

        global_interval[0] = np.maximum(global_interval, start_time)
        global_interval[1] = np.minimum(global_interval, end_time)

    # Check if all intervals overlap
    for i in range(len(intervals)):
        for j in range(i + 1, len(intervals)):
            overlap = intervals[j][0] <= intervals[i][1] and intervals[j][1] >= intervals[i][0]
            if not overlap:
                raise Exception("Signals from different stations do not overlap in time.")

    return tuple(global_interval)



if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
            Synchronizes raw signals from a specific modality across participants in a specific frequency. We first,
            resample (typically upsample) the signals and interpolate to generate equally spaced values, which we 
            can then align across participants.           
        """
    )

    parser.add_argument("--modality", type=str, required=True,
                        choices=["eeg", "ekg", "gsr", "fnirs", "gaze"],
                        help="Modality of the raw data.")
    parser.add_argument("--recording_freq", type=int, required=True,
                        help="Recording frequency. Frequency used when data was recorded.")
    parser.add_argument("--upsampled_freq", type=int, required=True,
                        help="Desired frequency after upsampling. Higher than the recording "
                             "frequency.")
    parser.add_argument("--n_jobs", type=int, required=False, default=NUM_PROCESSES,
                        help="Number of jobs for parallel processing.")
    parser.add_argument("--db_name", type=str, required=False, default="tomcat",
                        help="Name of the Postgres database where raw data is stored.")
    parser.add_argument("--db_user", type=str, required=False, default=USER,
                        help="User with granted reading permissions in the database.")
    parser.add_argument("--db_host", type=int, required=True,
                        help="Host where the database cluster is running.")
    parser.add_argument("--db_port", type=int, required=True,
                        help="Port where the database cluster is running.")
    parser.add_argument("--out_dir", type=str, required=False, default=OUT_DIR,
                        help="Directory where experiment folder structure containing semantic "
                             "labels must be saved.")
    parser.add_argument("--log_dir", type=str, required=False, default=LOG_DIR,
                        help="Directory where log files must be saved.")

    args = parser.parse_args()

    if args.recording_freq >= args.upsampled_freq:
        raise ValueError(f"Upsampled frequency ({args.upsampled_freq}) is smaller or "
                         f"equal than the recording frequency ({args.recording_freq}).")

    os.makedirs(args.log_dir, exist_ok=True)
    logging.basicConfig(
        level=logging.INFO,
        handlers=(
            logging.FileHandler(
                filename=f"{args.log_dir}/sync_raw_signals.log",
                mode="w",
            ),
            logging.StreamHandler(stream=sys.stderr),
        ),
    )

    reader = PostgresDataReader(
        signal_modality=MODALITIES.get(args.modality),
        db_name=args.db_name,
        db_user=args.db_user,
        db_host=args.db_host,
        db_port=args.db_port,
    )
    sync_raw_signals_in_parallel(
        data_reader=reader,
        source_frequency=args.recording_freq,
        target_frequency=args.upsampled_freq,
        num_jobs=args.n_jobs)
