#!/usr/bin/env python

import argparse
from sqlalchemy import create_engine
import logging
from logging import info, error
import sys
import subprocess

from datasette_interface.common.config import USER, SQLITE_DB_PATH
from datasette_interface.database.entity.base.base import Base

from datasette_interface.raw.process_base_tables import process_base_tables, recreate_base_tables
from datasette_interface.raw.process_rest_state_task_data import process_rest_state_task_data, \
    recreate_rest_state_table
from datasette_interface.raw.process_affective_task_data import process_affective_task_data, \
    recreate_affective_task_event_tables
from datasette_interface.raw.process_finger_tapping_task_data import \
    process_finger_tapping_task_data, \
    recreate_finger_tapping_task_observation_tables
from datasette_interface.raw.process_ping_pong_competitive_data import \
    process_ping_pong_competitive_task_data, \
    recreate_ping_pong_competitive_observation_tables
from datasette_interface.raw.process_ping_pong_cooperative_data import \
    process_ping_pong_cooperative_task_data, \
    recreate_ping_pong_cooperative_observation_tables
from datasette_interface.raw.process_minecraft_data import process_minecraft_data, \
    recreate_minecraft_tables
from datasette_interface.raw.process_fnirs_raw_data import process_fnirs_raw_data, \
    recreate_fnirs_raw_tables
from datasette_interface.raw.process_eeg_raw_data import process_eeg_raw_data, \
    recreate_eeg_raw_tables
from datasette_interface.raw.process_gaze_raw_data import process_gaze_raw_data, \
    recreate_gaze_raw_tables
from datasette_interface.raw.process_screen_capture_data import process_screen_capture_data, \
    recreate_screen_capture_tables
from datasette_interface.database.config import get_db, engine, SQLALCHEMY_DATABASE_URI
from datasette_interface.common.config import LOG_DIR, settings

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
    "screen_capture"
}


def recreate_tables(tables_to_recreate):
    if len(tables_to_recreate) == len(TABLES):
        info(f"Recreating all tables in {SQLALCHEMY_DATABASE_URI}.")
        Base.metadata.drop_all(engine, checkfirst=True)
        Base.metadata.create_all(engine)
    else:
        for table in tables_to_recreate:
            if table == "base":
                info(f"Recreating base tables in {database_info}.")
                recreate_base_tables()
            elif table == "rest_state":
                info(f"Recreating rest state table in {database_info}.")
                recreate_rest_state_table()
            elif table == "affective":
                info(f"Recreating affective task table in {database_info}.")
                recreate_affective_task_event_tables()
            elif table == "finger_tapping":
                info(f"Recreating finger tapping task table in {database_info}.")
                recreate_finger_tapping_task_observation_tables()
            elif table == "ping_pong_competitive":
                info(f"Recreating ping-pong competitive table in {database_info}.")
                recreate_ping_pong_competitive_observation_tables()
            elif table == "ping_pong_cooperative":
                info(f"Recreating ping-pong cooperative table in {database_info}.")
                recreate_ping_pong_cooperative_observation_tables()
            elif table == "minecraft":
                info(f"Recreating minecraft tables in {database_info}.")
                recreate_minecraft_tables()
            elif table == "fnirs":
                info(f"Recreating fnirs tables in {database_info}.")
                recreate_fnirs_raw_tables()
            elif table == "eeg":
                info(f"Recreating eeg tables in {database_info}.")
                recreate_eeg_raw_tables()
            elif table == "gaze":
                info(f"Recreating gaze tables in {database_info}.")
                recreate_gaze_raw_tables()
            elif table == "screen_capture":
                info(f"Recreating screen capture tables in {database_info}.")
                recreate_screen_capture_tables()


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
            Builds a PostgresSQL database from scratch. All the tables in the database will be 
            dropped, recreated and repopulated after running this script except if explicitly left 
            out by the no_* options. It assumes the cluster and database was previously created. 
            The script will not work if this is not true.           
        """
    )
    parser.add_argument("--include", type=str, default="all",
                        help=f"Comma-separated list of modalities to process among {TABLES}")
    parser.add_argument("--exclude", type=str, default="",
                        help=f"Comma-separated list of modalities to exclude among the ones in "
                             f"--include")

    args = parser.parse_args()

    if args.include.strip == "all":
        tables = TABLES
    else:
        tables = []
        for modality in args.include.split(","):
            modality = modality.strip()
            if modality not in TABLES:
                raise ValueError(f"Modality ({modality}) is invalid.")

            tables.append(modality)
        tables = set(tables)

    for modality in args.exclude.split(","):
        modality = modality.strip()
        if modality not in tables:
            raise ValueError(f"Modality ({modality}) not in --include ({tables}).")

        tables.remove(modality)

    if "base" in tables and len(tables) != len(TABLES):
        raise Exception(
            "It is not possible to recreate base tables without recreating all other tables "
            "altogether since all the content in the base tables will be erased and they are "
            "foreign keys to other tables.")

    if settings.drop_table:
        print(f"Recreate tables {tables}")
        # recreate_tables(tables)

    # populate_tables(tables)
    print(f"Reprocess tables {tables}")
    print(SQLALCHEMY_DATABASE_URI)
