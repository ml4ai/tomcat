#!/usr/bin/env python

import argparse
import os

from datasette_interface.database.config import SQLALCHEMY_DATABASE_URI
from datasette_interface.raw.process_affective_task_data import (
    process_affective_task_data,
)
from datasette_interface.raw.process_base_tables import process_base_tables
from datasette_interface.raw.process_eeg_raw_data import process_eeg_raw_data
from datasette_interface.raw.process_finger_tapping_task_data import (
    process_finger_tapping_task_data,
)
from datasette_interface.raw.process_fnirs_raw_data import process_fnirs_raw_data
from datasette_interface.raw.process_gaze_raw_data import process_gaze_raw_data
from datasette_interface.raw.process_minecraft_data import process_minecraft_data
from datasette_interface.raw.process_ping_pong_competitive_data import (
    process_ping_pong_competitive_task_data,
)
from datasette_interface.raw.process_ping_pong_cooperative_data import (
    process_ping_pong_cooperative_task_data,
)
from datasette_interface.raw.process_rest_state_task_data import (
    process_rest_state_task_data,
)
from datasette_interface.raw.process_screen_capture_data import (
    process_screen_capture_data,
)
from datasette_interface.raw.process_vocalics import process_vocalics

DATA_ENTITIES = {
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

DATA_ENTITY_TO_TABLES = {
    "base": "group_session, modality, participant, station, task, data_validity",
    "rest_state": "rest_state_task",
    "affective": "affective_task_event",
    "finger_tapping": "finger_tapping_task_observation",
    "ping_pong_competitive": "ping_pong_competitive_task_observation",
    "ping_pong_cooperative": "ping_pong_cooperative_task_observation",
    "minecraft": "minecraft_mission, minecraft_testbed_message",
    "fnirs": "fnirs_raw",
    "eeg": "eeg_raw",
    "gaze": "gaze_raw",
    "screen_capture": "screen_capture",
    "vocalics": "audio_vocalics",
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
        help=f"Comma-separated list of modalities to process among {DATA_ENTITIES}",
    )
    parser.add_argument(
        "--exclude",
        type=str,
        help="Comma-separated list of modalities to exclude among the ones in "
             "--include",
    )

    args = parser.parse_args()

    if args.include.strip() == "all":
        data_entities = DATA_ENTITIES
    else:
        data_entities = set()
        for data_entity in args.include.split(","):
            data_entity = data_entity.strip()
            if data_entity not in DATA_ENTITIES:
                raise ValueError(f"Data entity ({data_entity}) is invalid.")

            data_entities.add(data_entity)

    if args.exclude:
        for data_entity in args.exclude.split(","):
            data_entity = data_entity.strip()
            if len(data_entity) > 0 and data_entity not in data_entities:
                raise ValueError(
                    f"Data entity ({data_entity}) not in --include ({data_entities}).")

            data_entities.remove(data_entity)

    all_tables = ", ".join([DATA_ENTITY_TO_TABLES[data_entity] for data_entity in data_entities])
    answer = input(
        f"This operation may add data to the tables ({all_tables}) in the database "
        f"({SQLALCHEMY_DATABASE_URI}). Do you want to proceed? (y/n): "
    )
    if answer.lower() in ["y", "yes"]:
        populate_tables(data_entities)
    else:
        print("Operation aborted.")
