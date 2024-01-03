#!/usr/bin/env python

import argparse
import os

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
from datasette_interface.raw.process_vocalics import process_vocalics

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
    "vocalics",
}


def populate_tables(tables_to_process):
    # The order is important such that the signals have valid foreign keys to the base tables and
    # task tables.
    if "base" in tables_to_process:
        process_base_tables()
    if "rest_state" in tables_to_process:
        process_rest_state_task_data()
    if "affective" in tables_to_process:
        process_affective_task_data()
    if "finger_tapping" in tables_to_process:
        process_finger_tapping_task_data()
    if "ping_pong_competitive" in tables_to_process:
        process_ping_pong_competitive_task_data()
    if "ping_pong_cooperative" in tables_to_process:
        process_ping_pong_cooperative_task_data()
    if "minecraft" in tables_to_process:
        process_minecraft_data()
    if "screen_capture" in tables_to_process:
        process_screen_capture_data()
    if "vocalics" in tables_to_process:
        process_vocalics()
    if "fnirs" in tables_to_process:
        process_fnirs_raw_data()
    if "gaze" in tables_to_process:
        process_gaze_raw_data()
    if "eeg" in tables_to_process:
        process_eeg_raw_data()


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Populates the ToMCAT database with raw data. This script is incremental. No "
        "data is deleted from the database but added. It skips parsing data already "
        "inserted in the database. If one wants to override those, data must be "
        "deleted manually before calling this script."
    )
    parser.add_argument(
        "--include",
        type=str,
        default=os.getenv("TBS", "all"),
        help=f"Comma-separated list of modalities to process among {TABLES}",
    )
    parser.add_argument(
        "--exclude",
        type=str,
        help="Comma-separated list of modalities to exclude among the ones in "
        "--include",
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
