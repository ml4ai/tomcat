#!/usr/bin/env python

import argparse
import logging
import sys

from datasette_interface.common.config import LOG_DIR
from datasette_interface.database.config import SQLALCHEMY_DATABASE_URI
from datasette_interface.raw.process_affective_task_data import \
    process_affective_task_data
from datasette_interface.raw.process_base_tables import process_base_tables
from datasette_interface.raw.process_eeg_raw_data import process_eeg_raw_data
from datasette_interface.raw.process_finger_tapping_task_data import \
    process_finger_tapping_task_data
from datasette_interface.raw.process_fnirs_raw_data import \
    process_fnirs_raw_data
from datasette_interface.raw.process_gaze_raw_data import process_gaze_raw_data
from datasette_interface.raw.process_minecraft_data import \
    process_minecraft_data
from datasette_interface.raw.process_ping_pong_competitive_data import \
    process_ping_pong_competitive_task_data
from datasette_interface.raw.process_ping_pong_cooperative_data import \
    process_ping_pong_cooperative_task_data
from datasette_interface.raw.process_rest_state_task_data import \
    process_rest_state_task_data
from datasette_interface.raw.process_screen_capture_data import \
    process_screen_capture_data

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"{LOG_DIR}/recreate_database.log",
            mode="w",
        ),
        logging.StreamHandler(stream=sys.stderr),
    ),
)

TABLES = {
    "base",
    "rest_state",
    "affective",
    "finger_tapping",
    "ping_pong_competitive",
    "ping_pong_cooperative",
    "minecraft",
    "fnirs",
    "eeg",
    "gaze",
    "screen_capture",
}


def populate_tables(tables_to_process):
    # The others will have foreign key to the base tables
    if "base" in tables_to_process:
        process_base_tables()
    tables_to_process.remove("base")

    for table in tables_to_process:
        if table == "rest_state":
            process_rest_state_task_data()
        elif table == "affective":
            process_affective_task_data()
        elif table == "finger_tapping":
            process_finger_tapping_task_data()
        elif table == "ping_pong_competitive":
            process_ping_pong_competitive_task_data()
        elif table == "ping_pong_cooperative":
            process_ping_pong_cooperative_task_data()
        elif table == "minecraft":
            process_minecraft_data()
        elif table == "fnirs":
            process_fnirs_raw_data()
        elif table == "eeg":
            process_eeg_raw_data()
        elif table == "gaze":
            process_gaze_raw_data()
        elif table == "screen_capture":
            process_screen_capture_data()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
            Populates the ToMCAT database with raw data. This script is incremental. No data is 
            deleted from the database but added. It skips parsing data already inserted in 
            the database. If one wants to override those, data must be deleted manually before 
            calling this script.           
        """
    )
    parser.add_argument(
        "--include",
        type=str,
        default="all",
        help=f"Comma-separated list of modalities to process among {TABLES}",
    )
    parser.add_argument(
        "--exclude",
        type=str,
        help=f"Comma-separated list of modalities to exclude among the ones in "
        f"--include",
    )

    args = parser.parse_args()

    if args.include.strip() == "all":
        tables = TABLES
    else:
        tables = []
        for modality in args.include.split(","):
            modality = modality.strip()
            if modality not in TABLES:
                raise ValueError(f"Modality ({modality}) is invalid.")

            tables.append(modality)
        tables = set(tables)

    if args.exclude:
        for modality in args.exclude.split(","):
            modality = modality.strip()
            if len(modality) > 0 and modality not in tables:
                raise ValueError(f"Modality ({modality}) not in --include ({tables}).")

            tables.remove(modality)

    answer = input(
        f"This operation may add data to the tables ({tables}) in the database "
        f"({SQLALCHEMY_DATABASE_URI}). Do you want to proceed? (y/n): "
    )
    if answer.lower() in ["y", "yes"]:
        populate_tables(tables)
    else:
        print("Operation aborted.")
