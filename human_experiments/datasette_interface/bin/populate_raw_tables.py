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

TABLES = [
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
]


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
    for table in tables_to_process:
        if table == "base":
            process_base_tables()
        elif table == "rest_state":
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

    parser.add_argument("--no_base", action='store_true', help="Do not recreate base tables.")
    parser.add_argument("--no_rest_state", action='store_true', help="Do not recreate rest state "
                                                                     "tables.")
    parser.add_argument("--no_affective", action='store_true', help="Do not recreate affective "
                                                                    "task tables.")
    parser.add_argument("--no_finger_tapping", action='store_true', help="Do not recreate finger "
                                                                         "tapping task tables.")
    parser.add_argument("--no_ping_pong_comp", action='store_true',
                        help="Do not recreate ping-pong competitive tables.")
    parser.add_argument("--no_ping_pong_coop", action='store_true',
                        help="Do not recreate ping-pong cooperative tables.")
    parser.add_argument("--no_minecraft", action='store_true', help="Do not recreate minecraft "
                                                                    "tables.")
    parser.add_argument("--no_fnirs", action='store_true', help="Do not recreate fnirs tables.")
    parser.add_argument("--no_eeg", action='store_true', help="Do not recreate eeg tables.")
    parser.add_argument("--no_gaze", action='store_true', help="Do not recreate gaze tables.")
    parser.add_argument("--no_screen_capture", action='store_true', help="Do not recreate screen "
                                                                         "capture tables.")

    args = parser.parse_args()

    tables = TABLES.copy()
    if args.no_base:
        tables.remove("base")
    if args.no_rest_state:
        tables.remove("rest_state")
    if args.no_affective:
        tables.remove("affective")
    if args.no_finger_tapping:
        tables.remove("finger_tapping")
    if args.no_ping_pong_comp:
        tables.remove("ping_pong_competitive")
    if args.no_ping_pong_coop:
        tables.remove("ping_pong_cooperative")
    if args.no_minecraft:
        tables.remove("minecraft")
    if args.no_fnirs:
        tables.remove("fnirs")
    if args.no_eeg:
        tables.remove("eeg")
    if args.no_gaze:
        tables.remove("gaze")
    if args.no_screen_capture:
        tables.remove("screen_capture")

    if "base" in tables and len(tables) != len(TABLES):
        raise Exception(
            "It is not possible to recreate base tables without recreating all other tables "
            "altogether since all the content in the base tables will be erased and they are "
            "foreign keys to other tables. If you want to recreate all the tables, you have to "
            "explicitly not provide any no_* option.")

    if args.override:
        recreate_tables(tables)

    populate_tables(tables)
