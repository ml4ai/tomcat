#!/usr/bin/env python

import os
from logging import error, info

import pandas as pd
import pyxdf
from dateutil import parser
from tqdm import tqdm

from datasette_interface.common.config import settings
from datasette_interface.common.utils import (
    cd,
    convert_iso8601_timestamp_to_unix,
    convert_unix_timestamp_to_iso8601,
    is_directory_with_correct_image_creation_time,
    is_directory_with_unified_xdf_files,
    should_ignore_directory,
)
from datasette_interface.database.config import get_db
from datasette_interface.database.entity.signal.screen_capture import ScreenCapture
from datasette_interface.raw.common.process_raw_signals import get_signals


def process_directory_v1(group_session, image_table_class, image_dir):
    image_records = []
    for station in ("lion", "tiger", "leopard"):
        with cd(f"{group_session}/{station}/{image_dir}/"):
            # The images were renamed with their creation date, so the timestamp of each image
            # can be retrieved directly from their filename. # Images before 2022-11-07 do not
            # have their creation date saved. In this case we use their last modified date as
            # timestamp, which we can retrieve from the file's metadata.
            if is_directory_with_correct_image_creation_time(
                group_session
            ) and os.path.exists("outFile.csv"):
                # Sometimes outFile.csv exists but the image filenames are still the original ones.
                # When this is the case, we map the image filename to the timestamps in this file.
                filename_to_timestamp = pd.read_csv(
                    "outFile.csv", index_col=0, header=None, delimiter=";"
                )
                filename_to_timestamp.index.name = "filename"
                filename_to_timestamp.columns = ["timestamp"]
            else:
                info(
                    f"There isn't an outFile.csv in {station} or the creation timestamps are "
                    f"incorrect. The last file modification timestamp will be used as the "
                    f"record's timestamp."
                )
                filename_to_timestamp = None

            unique_id = 0
            sorted_filenames = sorted(os.listdir("../../.."))
            for filename in sorted_filenames:
                lb_idx = filename.rfind(".") + 1
                if filename[lb_idx:] != "png":
                    # Skip files that are not images.
                    continue

                if filename_to_timestamp is None:
                    # Get timestamp from file last modification date
                    timestamp_unix = os.path.getmtime(filename)
                    timestamp_iso8601 = convert_unix_timestamp_to_iso8601(
                        timestamp_unix
                    )
                    timestamp_origin = "modification"
                else:
                    timestamp_origin = "creation"
                    try:
                        # Get timestamp from the image filename
                        timestamp_iso8601 = filename[: filename.rfind(".")].replace(
                            "_", ":"
                        )

                        # Test if  filename is a valid timestamp
                        parser.parse(timestamp_iso8601)
                    except Exception:
                        # The filename does not contain a valid timestamp. Get the timestamp from
                        # the outFile.csv.
                        if filename in filename_to_timestamp.index:
                            timestamp_iso8601 = filename_to_timestamp.loc[
                                filename, "timestamp"
                            ]
                        else:
                            # The outFile.csv does not contain entries for all the images.
                            info(
                                f"[ANOMALY] No entry for {filename} in {station} was found in "
                                f"outFile.csv. Using the file modification timestamp instead."
                            )
                            timestamp_unix = os.path.getmtime(filename)
                            timestamp_iso8601 = convert_unix_timestamp_to_iso8601(
                                timestamp_unix
                            )
                            timestamp_origin = "modification"

                    timestamp_iso8601 = parser.parse(timestamp_iso8601).isoformat(
                        timespec="microseconds"
                    )
                    timestamp_unix = convert_iso8601_timestamp_to_unix(
                        timestamp_iso8601
                    )

                # Task and participant will be filled later in the labeling part.
                url = (
                    f"{settings.image_url_root_dir}/{group_session}/"
                    f"{station}/{image_dir}/{filename}"
                )
                image_record = image_table_class(
                    group_session_id=group_session,
                    id=unique_id,
                    task_id=None,
                    station_id=station,
                    participant_id=-1,
                    timestamp_unix=timestamp_unix,
                    timestamp_iso8601=timestamp_iso8601,
                    filename=filename,
                    url=url,
                    timestamp_origin=timestamp_origin,
                )
                image_records.append(image_record)

                unique_id += 1

    return image_records


def process_directory_v2(
    group_session, image_table_class, image_type, xdf_signal_name, image_dir
):
    """Process directory assuming unified XDF files."""

    records = []
    with cd(f"{group_session}/lsl"):
        max_ids = {}
        for block_num, xdf_file in enumerate(("block_1.xdf", "block_2.xdf")):
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
                station = stream["info"]["hostname"][0]
                next_id = max_ids.get(station, -1) + 1
                station = stream["info"]["hostname"][0]
                signals = get_signals(
                    stream,
                    group_session,
                    station,
                    next_id,
                    lambda x: ["filename"],
                    slice_series_fn=lambda x: x,
                )
                tmp = []
                for signal_dict in signals:
                    url = (
                        f"{settings.image_url_root_dir}/{group_session}/{station}/{image_dir}/"
                        f"block_{block_num + 1}/{signal_dict['filename']}"
                    )
                    tmp.append(
                        image_table_class(
                            **signal_dict, timestamp_origin="lsl", url=url
                        )
                    )

                records.extend(tmp)
                max_ids[station] = tmp[-1].id

    return records


def insert_raw_unlabeled_data(
    image_table_class, image_type, xdf_signal_name, image_dir
):
    info("Inserting unlabeled data.")
    with cd(settings.experiment_root_dir):
        directories_to_process = [
            directory
            for directory in os.listdir(".")
            if not should_ignore_directory(directory)
        ]

        db = next(get_db())
        processed_group_sessions = set(
            [
                s[0]
                for s in db.query(ScreenCapture.group_session_id)
                .distinct(ScreenCapture.group_session_id)
                .all()
            ]
        )

        for group_session in tqdm(sorted(directories_to_process), unit="directories"):
            if group_session in processed_group_sessions:
                info(
                    f"Found saved {image_type} records for {group_session} in the database. "
                    f"Skipping group session."
                )
                continue

            info(f"Processing directory {group_session}")
            if not is_directory_with_unified_xdf_files(group_session):
                image_records = process_directory_v1(
                    group_session, image_table_class, image_dir
                )
            else:
                image_records = process_directory_v2(
                    group_session,
                    image_table_class,
                    image_type,
                    xdf_signal_name,
                    image_dir,
                )

            info("Adding records to the database.")
            db.add_all(image_records)
            db.commit()
        db.close()
