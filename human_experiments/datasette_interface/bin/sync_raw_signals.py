#!/usr/bin/env python

import argparse
import logging
import os
from functools import partial
from logging import info
from multiprocessing import Pool
from typing import List

import mne
import numpy as np
from sqlalchemy import create_engine, select
from sqlalchemy.orm import Session
from tqdm import tqdm

from datasette_interface.common.config import LOG_DIR
from datasette_interface.common.constants import STATIONS
from datasette_interface.database.config import (SQLALCHEMY_DATABASE_URI,
                                                 engine, get_db)
from datasette_interface.database.entity.base.group_session import GroupSession
from datasette_interface.derived.helper.factory import create_modality_helper
from datasette_interface.derived.main_clock import get_main_clock_timestamps


def sync_raw_signals_in_parallel(
    modality: str,
    clock_frequency: float,
    up_sample_scale: float,
    num_jobs: int,
    buffer: int = 60,
):
    """
    Synchronizes data from one modality for different group sessions in parallel.

    :param modality: modality of the signal.
    :param clock_frequency: frequency of the main clock signals will be to be in-sync with.
    :param up_sample_scale: by how much to up-sample the raw signal before interpolating with the
        main clock.
    :param num_jobs: number of jobs for parallelization.
    :param buffer: a buffer in seconds to be sure we don't lose any signal data.
    """
    db = next(get_db())
    all_group_sessions = db.scalars(select(GroupSession.id)).all()
    helper = create_modality_helper(modality, None, None, engine)
    processed_group_sessions = helper.get_processed_group_sessions(clock_frequency)
    remaining_group_sessions = sorted(
        list(set(all_group_sessions).difference(set(processed_group_sessions)))
    )
    db.close()

    effective_num_jobs = min(num_jobs, len(remaining_group_sessions))
    group_session_batches = np.array_split(remaining_group_sessions, effective_num_jobs)

    job_fn = partial(
        sync_raw_signals_single_job,
        modality=modality,
        clock_frequency=clock_frequency,
        up_sample_scale=up_sample_scale,
        buffer=buffer,
        # For parallel execution, each single job must have their own connection to the DB to
        # avoid putting the server-side session into a state that the client no longer knows how
        # to interpret
        use_global_db_connection=(effective_num_jobs == 1),
    )

    print(
        f"Synchronizing {modality} signals from {len(remaining_group_sessions)} group sessions."
    )
    print("\n".join(remaining_group_sessions))
    print("")
    if effective_num_jobs == 1:
        for group_session in tqdm(remaining_group_sessions):
            job_fn([group_session])
    else:
        with Pool(processes=num_jobs) as pool:
            list(
                tqdm(
                    pool.imap(job_fn, group_session_batches),
                    total=len(group_session_batches),
                )
            )


def sync_raw_signals_single_job(
    group_sessions: List[str],
    modality: str,
    clock_frequency: float,
    up_sample_scale: float,
    buffer: int,
    use_global_db_connection: bool,
):
    """
    Synchronizes data from one modality for different group sessions in sequence.

    :param group_sessions: group sessions to process.
    :param modality: modality of the signal.
    :param clock_frequency: frequency of the main clock signals will be to be in-sync with.
    :param up_sample_scale: by how much to up-sample the raw signal before interpolating with the
        main clock.
    :param buffer: a buffer in seconds to be sure we don't lose any signal data.
    :param use_global_db_connection: whether ti use a global engine or a specific one for the job.
    """
    if use_global_db_connection:
        db_engine = engine
        db = next(get_db())
    else:
        db_engine = create_engine(SQLALCHEMY_DATABASE_URI)
        db = Session(db_engine)

    for group_session in group_sessions:
        print(f"Processing {group_session}")
        log_filepath = (
            f"{LOG_DIR}/sync_raw_{modality}_{group_session}_{clock_frequency}.log"
        )
        mne_log_filepath = (
            f"{LOG_DIR}/sync_raw_{modality}_{group_session}_{clock_frequency}_"
            f"mne.log"
        )
        log_format = "%(asctime)s - %(levelname)s - %(message)s"
        logging.basicConfig(
            level=logging.INFO,
            format=log_format,
            handlers=(
                logging.FileHandler(
                    filename=log_filepath,
                    mode="w",
                ),
            ),
            force=True,
        )

        mne.set_log_file(mne_log_filepath, output_format=log_format, overwrite=False)
        info(f"Processing group session {group_session}.")

        clock_timestamps = get_main_clock_timestamps(
            group_session=group_session,
            clock_frequency=clock_frequency,
            buffer=buffer,
            db=db,
        )

        for station in STATIONS:
            info(f"Processing station {station}.")
            modality_helper = create_modality_helper(
                modality, group_session, station, db_engine
            )

            if modality_helper.has_saved_sync_data(clock_frequency):
                info(
                    f"Found synchronized {modality} signals for {group_session}, {station} with "
                    f"clock frequency {clock_frequency} in the database. Skipping."
                )
                continue

            info("Loading data.")
            modality_helper.load_data()

            if modality_helper.is_data_empty():
                info(
                    f"No raw {modality} signals found for {group_session}, {station}. Skipping."
                )
                continue

            info("Filtering.")
            modality_helper.filter()

            info("Up-sampling.")
            modality_helper.up_sample(up_sample_scale)

            info("Synchronizing with main clock.")
            modality_helper.sync_to_clock(clock_frequency, clock_timestamps)

            info("Persisting synchronized data.")
            modality_helper.save_synced_data()

    db.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Filters and synchronizes raw signals with a main clock. The main clock has "
        "an associated frequency, start and end timestamps. The frequency is determined at "
        "execution time. The start timestamp is defined by 1 minute before the start of the "
        "rest state task (first task in the experimental procedure) and end timestamp "
        "defined by 1 minute after the end of the last Minecraft trial."
    )

    parser.add_argument(
        "--modality",
        type=str,
        choices=["eeg", "fnirs"],
        help="The modality of the signal.",
    )
    parser.add_argument(
        "--clock_frequency", type=int, help="The frequency of the main clock."
    )
    parser.add_argument(
        "--up_sample_scale",
        type=int,
        default=10,
        help="By how much to up-sample the signal before interpolating with the "
        "main clock time scale.",
    )
    parser.add_argument(
        "--num_jobs",
        type=int,
        default=os.getenv("N_JOBS", 1),
        help="Number of jobs for parallel processing.",
    )
    parser.add_argument(
        "--buffer",
        type=int,
        default=60,
        help="Buffer (in seconds) to include in the extremes of the main clock. "
        "For instance, a buffer of 60 will synchronize data to a clock that "
        "starts 1 minute (buffer) before the beginning of the rest state task "
        "and finishes 1 minute (buffer) after the end of the last minecraft "
        "trial.",
    )
    args = parser.parse_args()

    sync_raw_signals_in_parallel(
        modality=args.modality,
        clock_frequency=args.clock_frequency,
        up_sample_scale=args.up_sample_scale,
        num_jobs=args.num_jobs,
        buffer=args.buffer,
    )
