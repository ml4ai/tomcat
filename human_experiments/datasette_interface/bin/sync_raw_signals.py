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

from datasette_interface.derived.helper.modality import ModalityHelper
from multiprocessing import Pool
from datasette_interface.database.entity.base.group_session import GroupSession
from sqlalchemy import select
from datasette_interface.database.config import get_db
from datasette_interface.derived.main_clock import get_main_clock_timestamps
from datasette_interface.common.constants import STATIONS
from datasette_interface.derived.helper.factory import create_modality_helper
from datasette_interface.common.config import LOG_DIR
import mne


def sync_raw_signals_in_parallel(modality: str,
                                 clock_frequency: float,
                                 up_sample_scale: float,
                                 num_jobs: int,
                                 buffer: int = 60):
    """
    Synchronizes data from one modality for different group sessions in parallel.

    :param modality: modality of the signal.
    :param clock_frequency: frequency of the main clock signals will be to be in-sync with.
    :param up_sample_scale: by how much to up-sample the raw signal before interpolating with the
        main clock.
    :param num_jobs: number of jobs for parallelization.
    :param buffer: a buffer in seconds to be sure we don't lose any signal data.
    """
    job_fn = partial(sync_raw_signals_single_job,
                     modality=modality,
                     clock_frequency=clock_frequency,
                     up_sample_scale=up_sample_scale,
                     buffer=buffer)

    db = next(get_db())
    group_sessions = db.scalars(select(GroupSession.id)).all()[-2:]
    db.close()

    effective_num_jobs = min(num_jobs, len(group_sessions))
    group_session_batches = np.array_split(group_sessions, effective_num_jobs)

    print(f"Synchronizing signals from {len(group_sessions)} group sessions.")
    if effective_num_jobs == 1:
        for group_session in tqdm(group_sessions):
            job_fn([group_session])
    else:
        with Pool(processes=num_jobs) as pool:
            list(tqdm(pool.imap(job_fn, group_session_batches), total=len(group_session_batches)))


def sync_raw_signals_single_job(group_sessions: List[str],
                                modality: str,
                                clock_frequency: float,
                                up_sample_scale: float,
                                buffer: int):
    """
    Synchronizes data from one modality for different group sessions in sequence.

    :param group_sessions: group sessions to process.
    :param modality: modality of the signal.
    :param clock_frequency: frequency of the main clock signals will be to be in-sync with.
    :param up_sample_scale: by how much to up-sample the raw signal before interpolating with the
        main clock.
    :param buffer: a buffer in seconds to be sure we don't lose any signal data.
    """
    for group_session in group_sessions:
        log_filepath = f"{LOG_DIR}/sync_raw_{modality}_{group_session}_{clock_frequency}.log"
        mne_log_filepath = f"{LOG_DIR}/sync_raw_{modality}_{group_session}_{clock_frequency}_" \
                           f"mne.log"
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
            force=True
        )
        logger = logging.getLogger()
        mne.set_log_file(mne_log_filepath,
                         output_format=log_format,
                         overwrite=False)
        logger.info(f"Processing group session {group_session}.")

        clock_timestamps = get_main_clock_timestamps(group_session=group_session,
                                                     clock_frequency=clock_frequency,
                                                     buffer=buffer)

        for station in STATIONS:
            logger.info(f"Processing station {station}.")
            modality_helper = create_modality_helper(modality, group_session, station)

            if modality_helper.has_saved_sync_data(clock_frequency):
                logger.info(
                    f"Found synchronized {modality} signals for {group_session}, {station} with "
                    f"clock frequency {clock_frequency} in the database. Skipping.")
                continue

            logger.info(f"Loading data.")
            modality_helper.load_data()

            logger.info(f"Filtering.")
            modality_helper.filter()

            logger.info(f"Up-sampling.")
            modality_helper.up_sample(up_sample_scale)

            logger.info(f"Synchronizing with main clock.")
            modality_helper.sync_to_clock(clock_frequency, clock_timestamps)

            logger.info(f"Persisting synchronized data.")
            modality_helper.save_synced_data()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
                Filters and synchronizes raw signals with a main clock. The main clock has an 
                associated frequency, start and end timestamps. The frequency is determined at 
                execution time. The start timestamp is defined by 1 minute before the start of the 
                rest state task (first task in the experimental procedure) and end timestamp defined
                by 1 minute after the end of the last Minecraft trial.       
            """
    )

    parser.add_argument("--modality", type=str, choices=["eeg", "fnirs"],
                        help="The modality of the signal.")
    parser.add_argument("--clock_frequency", type=int,
                        help="The frequency of the main clock.")
    parser.add_argument("--up_sample_scale", type=int, default=10,
                        help="By how much to up-sample the signal before interpolating with the "
                             "main clock time scale.")
    parser.add_argument("--num_jobs", type=int, default=1,
                        help="Number of jobs for parallel processing.")
    parser.add_argument("--buffer", type=int, default=60,
                        help="Buffer (in seconds) to include in the extremes of the main clock. "
                             "For instance, a buffer of 60 will synchronize data to a clock that "
                             "starts 1 minute (buffer) before the beginning of the rest state task "
                             "and finishes 1 minute (buffer) after the end of the last minecraft "
                             "trial.")
    args = parser.parse_args()

    sync_raw_signals_in_parallel(
        modality=args.modality,
        clock_frequency=args.clock_frequency,
        up_sample_scale=args.up_sample_scale,
        num_jobs=args.num_jobs,
        buffer=args.buffer)
