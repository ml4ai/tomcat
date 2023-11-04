import argparse
from functools import partial
import logging
from typing import List, Tuple
import os
import sys
import math
import numpy as np
import pandas as pd

from tqdm import tqdm

from data_pre_processing.common.config import NUM_PROCESSES, LOG_DIR, OUT_DIR, USER
from data_pre_processing.common.data_source.db import PostgresDB
from data_pre_processing.signal.reader.data_reader import DataReader, PostgresDataReader
from data_pre_processing.signal.writer.data_writer import DataWriter, PostgresDataWriter
from data_pre_processing.common.utils import convert_unix_timestamp_to_iso8601
from data_pre_processing.signal.entity.factory import create_modality
from data_pre_processing.signal.entity.modality import Modality

from multiprocessing import Pool


def sync_raw_signals_in_parallel(data_reader: DataReader,
                                 data_writer: DataWriter,
                                 source_frequency: int,
                                 upsampling_frequency: int,
                                 final_frequency: int,
                                 num_jobs: int):
    """
    Synchronizes data from one modality for different group sessions in parallel.

    :param data_reader: object responsible for reading the data to be synchronized.
    :param data_writer: object responsible for writing the synchronized data.
    :param source_frequency: theoretical frequency of the original data.
    :param target_frequency: final frequency of the synchronized data.
    :param num_jobs: number of jobs for parallelization.
    """
    job_fn = partial(sync_raw_signals_single_job,
                     data_reader=data_reader,
                     data_writer=data_writer,
                     source_frequency=source_frequency,
                     upsampling_frequency=upsampling_frequency,
                     final_frequency=final_frequency)
    #
    # group_sessions = data_reader.read_group_sessions()
    # with Pool(processes=num_jobs) as pool:
    #     list(tqdm(pool.imap(job_fn, group_sessions), total=len(group_sessions)))
    job_fn(["exp_2022_10_04_09"])


def sync_raw_signals_single_job(group_sessions: List[str],
                                data_reader: DataReader,
                                data_writer: DataWriter,
                                source_frequency: int,
                                upsampling_frequency: int,
                                final_frequency: int):
    for group_session in group_sessions:
        print(f"Processing group session {group_session}.")
        data = data_reader.read(group_session)

        common_start_time, common_end_time = get_overlapping_window_across_stations(data)

        # Round start and end time points to the closest second.
        common_start_time = math.ceil(common_start_time)
        common_end_time = math.floor(common_end_time)

        downsampling_factor = upsampling_frequency / final_frequency

        for station in sorted(data["station"].unique()):
            print(f"Processing station {station}.")

            raw_station_data = data[data["station"] == station].drop(
                columns=["group_session", "station"])

            print("Upsampling")
            upsampling_factor = upsampling_frequency / source_frequency
            upsampled_data = Modality.upsample(raw_station_data, upsampling_factor)

            print("Filtering")
            filtered_data = data_reader.signal_modality.filter(upsampled_data,
                                                               upsampling_frequency)

            print("Interpolating unfiltered")
            interpolated_data = Modality.interpolate(
                data=upsampled_data,
                start_time=common_start_time,
                end_time=common_end_time,
                target_frequency=upsampling_frequency
            )

            print("Interpolating filtered")
            filtered_interpolated_data = Modality.interpolate(
                data=filtered_data,
                start_time=common_start_time,
                end_time=common_end_time,
                target_frequency=upsampling_frequency
            )

            print("Downsampling unfiltered")
            downsampled_unfiltered_data = Modality.downsample(interpolated_data,
                                                              downsampling_factor)

            print("Downsampling filtered")
            downsampled_filtered_data = Modality.downsample(filtered_interpolated_data,
                                                            downsampling_factor)

            # Add extra info
            for df in [downsampled_unfiltered_data, downsampled_filtered_data]:
                df["group_session"] = group_session
                df["station"] = station
                df["id"] = np.arange(len(df), dtype=int)
                df["frequency"] = final_frequency
                df["timestamp_iso8601"] = df["timestamp_unix"].apply(
                    convert_unix_timestamp_to_iso8601)

            # We save to the database per station to reduce the overhead since this essentially
            # can multiply the number of raw samples by a lot.
            print("Saving unfiltered data")
            data_writer.write_unfiltered(downsampled_unfiltered_data)

            print("Saving filtered data")
            data_writer.write_filtered(downsampled_filtered_data)


def get_overlapping_window_across_stations(data: pd.DataFrame) -> Tuple[float, float]:
    """
    Returns a tuple containing overlapping start time and end time across data signals from
    different stations.

    :param data: data signals.
    :return: a tuple containing the latest initial time and earliest end time of data signals
    across different stations.
    """

    global_interval = None
    intervals = []
    for station in data["station"].unique():
        start_time = data[data["station"] == station]["timestamp_unix"].min()
        end_time = data[data["station"] == station]["timestamp_unix"].max()
        intervals.append((start_time, end_time))

        if global_interval is None:
            global_interval = [start_time, end_time]
        else:
            global_interval[0] = np.maximum(global_interval[0], start_time)
            global_interval[1] = np.minimum(global_interval[1], end_time)

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
    parser.add_argument("--upsampling_freq", type=int, required=True,
                        help="Desired frequency after upsampling. Higher than the recording "
                             "frequency.")
    parser.add_argument("--final_freq", type=int, required=True,
                        help="Desired frequency after downsampling. This is the final frequency the "
                             "signals will have when saved.")
    parser.add_argument("--n_jobs", type=int, required=False, default=NUM_PROCESSES,
                        help="Number of jobs for parallel processing.")
    parser.add_argument("--db_name", type=str, required=False, default="tomcat",
                        help="Name of the Postgres database where raw data is stored.")
    parser.add_argument("--db_user", type=str, required=False, default=USER,
                        help="User with granted reading permissions in the database.")
    parser.add_argument("--db_host", type=str, required=False, default="localhost",
                        help="Host where the database cluster is running.")
    parser.add_argument("--db_port", type=int, required=False, default=5433,
                        help="Port where the database cluster is running.")
    parser.add_argument("--db_passwd", type=str, required=False, default="",
                        help="Password to connect to the database.")
    parser.add_argument("--out_dir", type=str, required=False, default=OUT_DIR,
                        help="Directory where experiment folder structure containing semantic "
                             "labels must be saved.")
    parser.add_argument("--log_dir", type=str, required=False, default=LOG_DIR,
                        help="Directory where log files must be saved.")

    args = parser.parse_args()

    if args.recording_freq >= args.upsampling_freq:
        raise ValueError(f"Upsampling frequency ({args.upsampled_freq}) is smaller or "
                         f"equal than the recording frequency ({args.recording_freq}).")

    if args.final_freq >= args.upsampling_freq:
        raise ValueError(f"Final frequency ({args.final_freq}) is larger or "
                         f"equal than the upsampling frequency ({args.recording_freq}).")

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

    db = PostgresDB(db_name=args.db_name,
                    db_user=args.db_user,
                    db_host=args.db_host,
                    db_port=args.db_port,
                    db_passwd=args.db_passwd)
    modality = create_modality(modality=args.modality)
    reader = PostgresDataReader(
        signal_modality=modality,
        db=db
    )
    writer = PostgresDataWriter(
        signal_modality=modality,
        db=db
    )

    print("Create tables")
    modality.get_data_mode_table_class("filtered").__table__.create(db.create_engine(),
                                                                    checkfirst=True)
    modality.get_data_mode_table_class("unfiltered").__table__.create(db.create_engine(),
                                                                      checkfirst=True)

    sync_raw_signals_in_parallel(
        data_reader=reader,
        data_writer=writer,
        source_frequency=args.recording_freq,
        upsampling_frequency=args.upsampling_freq,
        final_frequency=args.final_freq,
        num_jobs=args.n_jobs)
