#!/usr/bin/env python


import os
import time
from logging import info, error

import pandas as pd
import pyxdf
from dateutil import parser
from sqlalchemy.orm import Session
from tqdm import tqdm

from entity.signal.screen_capture import ScreenCapture
from process_raw_signals import get_signals
from utils import (
    cd,
    should_ignore_directory,
    convert_iso8601_timestamp_to_unix,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_unified_xdf_files,
)


def process_directory_v1(group_session, image_table_class):
    image_records = []
    for station in ("lion", "tiger", "leopard"):
        with cd(f"{group_session}/{station}/screenshots/"):
            # The images were renamed with their creation date, so the timestamp of each image
            # can be retrieved directly from their filename. # Images before 2022-11-07 do not
            # have their creation date saved. In this case we use their last modified date as
            # timestamp, which we can retrieve from the file's metadata.
            if os.path.exists("outFile.csv"):
                # Sometimes outFile.csv exists but the image filenames are still the original ones.
                # When this is the case, we map the image filename to the timestamps in this file.
                filename_to_timestamp = pd.read_csv("outFile.csv", index_col=0, header=None)
                if len(filename_to_timestamp.columns) == 0:
                    # Some of the files have ";" as delimiter
                    filename_to_timestamp = pd.read_csv("outFile.csv", index_col=0, header=None,
                                                        delimiter=";")
                filename_to_timestamp.index.name = "filename"
                filename_to_timestamp.columns = ["timestamp"]
            else:
                info(f"There isn't an outFile.csv in {station}. The last file modification "
                     f"timestamp will be used as the record's timestamp.")
                filename_to_timestamp = None

            unique_id = 0
            sorted_filenames = sorted(os.listdir("."))
            for filename in sorted_filenames:
                if filename[filename.rfind(".") + 1:] != "png":
                    # Skip files that are not images.
                    continue

                if filename_to_timestamp is not None:
                    try:
                        # Get timestamp from the filename
                        timestamp_iso8601 = filename[:filename.rfind(".")]
                        timestamp_unix = convert_iso8601_timestamp_to_unix(timestamp_iso8601)
                    except Exception:
                        # The filename does not contain a valid timestamp. Get the timestamp from
                        # the outFile.csv.
                        timestamp_iso8601 = filename_to_timestamp.loc[filename, "timestamp"]
                        timestamp_unix = convert_iso8601_timestamp_to_unix(timestamp_iso8601)

                    # Convert back to ISO to transform from MST to UTC
                    timestamp_iso8601 = convert_unix_timestamp_to_iso8601(timestamp_unix)
                else:
                    timestamp_unix = os.path.getmtime(filename)
                    timestamp_iso8601 = convert_unix_timestamp_to_iso8601(timestamp_unix)

                # Task and participant will be filled later in the labeling part.
                image_record = image_table_class(
                    group_session_id=group_session,
                    id=unique_id,
                    task_id=None,
                    station_id=station,
                    participant_id=-1,
                    timestamp_unix=timestamp_unix,
                    timestamp_iso8601=timestamp_iso8601,
                    filename=filename
                )
                image_records.append(image_record)

                unique_id += 1

    return image_records


def process_directory_v2(group_session, image_table_class, image_type, xdf_signal_name):
    """Process directory assuming unified XDF files."""

    records = []
    with cd(f"{group_session}/lsl"):
        max_ids = {}
        for xdf_file in ("block_1.xdf", "block_2.xdf"):
            try:
                streams, header = pyxdf.load_xdf(
                    xdf_file, select_streams=[{"name": xdf_signal_name}]
                )
            except ValueError as e:
                error(f"[MISSING DATA]: No {image_type} stream found in {xdf_file}!")
                print(e)
                continue
            except Exception as e:
                error(f"[MISSING DATA]: {e}")
                continue

            for stream in streams:
                # For v2, the signals are split in two files (block1 and 2). When processing block2
                # entries, we need to make sure we start the ID taking into account the last ID in
                # the block 1 files for the same station, otherwise we get a duplicate key error
                # during insertion.
                station = stream["info"]["hostname"]
                next_id = max_ids.get(station, - 1) + 1
                station = stream["info"]["hostname"][0]
                tmp = get_signals(stream, group_session, station, next_id, image_table_class,
                                  lambda x: ["filename"], slice_series_fn=lambda x: x)
                records.extend(tmp)
                max_ids[station] = tmp[-1].id

    return records


def insert_raw_unlabeled_data(database_engine, override, image_table_class, image_type,
                              xdf_signal_name):
    info(f"Inserting unlabeled data.")
    with cd("/tomcat/data/raw/LangLab/experiments/study_3_pilot/group"):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        with Session(database_engine) as database_session:
            processed_group_sessions = set(
                [s[0] for s in
                 database_session.query(ScreenCapture.group_session_id).distinct(
                     ScreenCapture.group_session_id).all()])

            for group_session in tqdm(sorted(directories_to_process), unit="directories"):
                if not override and group_session in processed_group_sessions:
                    info(
                        f"Found saved {image_type} records for {group_session} in the database. Skipping group session.")
                    continue

                info(f"Processing directory {group_session}")
                if not is_directory_with_unified_xdf_files(group_session):
                    image_records = process_directory_v1(group_session, image_table_class)
                else:
                    image_records = process_directory_v2(group_session, image_table_class,
                                                         image_type,
                                                         xdf_signal_name)

                info("Adding records to the database.")
                database_session.add_all(image_records)
                database_session.commit()
