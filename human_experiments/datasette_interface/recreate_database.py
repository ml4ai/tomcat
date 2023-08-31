#!/usr/bin/env python

import argparse
from sqlalchemy import create_engine
import logging
from logging import info
import sys

from config import USER
from entity.base.base import Base

from process_base_tables import process_base_tables, recreate_base_tables
from process_rest_state_task_data import process_rest_state_task_data, recreate_rest_state_table
from process_affective_task_data import process_affective_task_data, recreate_affective_task_event_table
from process_finger_tapping_task_data import process_finger_tapping_task_data, \
    recreate_finger_tapping_task_observation_table

logging.basicConfig(
    level=logging.INFO,
    handlers=(
        logging.FileHandler(
            filename=f"/space/{USER}/tomcat/recreate_database.log",
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
    "testbed_message",
    "fnirs",
    "eeg",
    "gaze"
]


def recreate_tables(tables_to_recreate, database_engine):
    if len(tables_to_recreate) == len(TABLES):
        info(f"Recreating all tables in {database_info}.")
        Base.metadata.drop_all(database_engine, checkfirst=True)
        Base.metadata.create_all(database_engine)
    else:
        for table in tables_to_recreate:
            if table == "base":
                info(f"Recreating base tables in {database_info}.")
                recreate_base_tables(database_engine)
            elif table == "rest_state":
                info(f"Recreating rest state table in {database_info}.")
                recreate_rest_state_table(database_engine)
            elif table == "affective":
                info(f"Recreating affective task table in {database_info}.")
                recreate_affective_task_event_table(database_engine)
            elif table == "finger_tapping":
                info(f"Recreating finger tapping task table in {database_info}.")
                recreate_finger_tapping_task_observation_table(database_engine)
            elif table == "ping_pong_competitive":
                info(f"Recreating ping-pong competitive table in {database_info}.")
                # recreate_finger_tapping_task_observation_table(database_engine)
            elif table == "ping_pong_cooperative":
                info(f"Recreating ping-pong cooperative table in {database_info}.")
                # recreate_finger_tapping_task_observation_table(database_engine)
            elif table == "testbed_message":
                info(f"Recreating testbed message table in {database_info}.")
                # recreate_finger_tapping_task_observation_table(database_engine)
            elif table == "fnirs":
                info(f"Recreating fnirs table in {database_info}.")
                # recreate_finger_tapping_task_observation_table(database_engine)
            elif table == "eeg":
                info(f"Recreating eeg table in {database_info}.")
                # recreate_finger_tapping_task_observation_table(database_engine)
            elif table == "gaze":
                info(f"Recreating gaze table in {database_info}.")
                # recreate_finger_tapping_task_observation_table(database_engine)


def populate_tables(tables_to_process, database_engine):
    for table in tables_to_process:
        if table == "base":
            process_base_tables(database_engine)
        elif table == "rest_state":
            process_rest_state_task_data(database_engine)
        elif table == "affective":
            process_affective_task_data(database_engine)
        elif table == "finger_tapping":
            process_finger_tapping_task_data(database_engine)
        elif table == "ping_pong_competitive":
            pass
            # process_finger_tapping_task_data(database_engine)
        elif table == "ping_pong_cooperative":
            pass
            # process_finger_tapping_task_data(database_engine)
        elif table == "testbed_message":
            pass
            # process_finger_tapping_task_data(database_engine)
        elif table == "fnirs":
            pass
            # process_finger_tapping_task_data(database_engine)
        elif table == "eeg":
            pass
            # process_finger_tapping_task_data(database_engine)
        elif table == "gaze":
            pass
            # process_finger_tapping_task_data(database_engine)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
            Builds a PostgresSQL database from scratch. All the tables in the database will be dropped, 
            recreated and repopulated after running this script except if explicitly left out by the no_* options. 
            It assumes the cluster and database was previously created. The script will not work if this is not true.           
        """
    )

    parser.add_argument("--db_user", type=str, required=False, default=USER,
                        help="User with granted writing permissions in the database.")
    parser.add_argument("--db_port", type=int, required=True,
                        help="Port where the cluster is running. Make sure to set the port to your local cluster.")
    parser.add_argument("--db_name", type=str, required=False, default=f"{USER}_tomcat",
                        help="Database name. Make sure the database was previously created before running this script.")
    parser.add_argument("--db_passwd", type=str, required=True, help="Password to connect to the database.")
    parser.add_argument("--no_base", action='store_true', help="Do not reprocess base tables.")
    parser.add_argument("--no_rest_state", action='store_true', help="Do not reprocess rest state table.")
    parser.add_argument("--no_affective", action='store_true', help="Do not reprocess affective task table.")
    parser.add_argument("--no_finger_tapping", action='store_true', help="Do not reprocess finger tapping task table.")
    parser.add_argument("--no_ping_pong_comp", action='store_true',
                        help="Do not reprocess ping-pong competitive table.")
    parser.add_argument("--no_ping_pong_coop", action='store_true',
                        help="Do not reprocess ping-pong cooperative table.")
    parser.add_argument("--no_testbed", action='store_true', help="Do not reprocess testbed message table.")
    parser.add_argument("--no_fnirs", action='store_true', help="Do not reprocess fnirs table.")
    parser.add_argument("--no_eeg", action='store_true', help="Do not reprocess eeg table.")
    parser.add_argument("--no_gaze", action='store_true', help="Do not reprocess gaze table.")

    args = parser.parse_args()

    database_info = f"{args.db_user}:{args.db_name}@localhost:{args.db_port}"
    connection_string = f"postgresql+psycopg2://{database_info}/{args.db_passwd}"
    engine = create_engine(connection_string)

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
    if args.no_testbed:
        tables.remove("testbed_message")
    if args.no_fnirs:
        tables.remove("fnirs")
    if args.no_eeg:
        tables.remove("eeg")
    if args.no_gaze:
        tables.remove("gaze")

    recreate_tables(tables, engine)
    populate_tables(tables, engine)
