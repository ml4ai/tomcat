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
    "rest_state"
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
                info(f"Recreating rest state tables in {database_info}.")
                recreate_rest_state_table(database_engine)


def populate_tables(tables_to_process, database_engine):
    for table in tables_to_process:
        if table == "base":
            process_base_tables(database_engine)
        elif table == "rest_state":
            process_rest_state_task_data(database_engine)


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
    parser.add_argument("--no_base", action='store_false', help="Do not reprocess base tables.")
    parser.add_argument("--no_rest_state", action='store_false', help="Do not reprocess rest state tables.")

    args = parser.parse_args()

    database_info = f"{args.db_user}:{args.db_name}@localhost:{args.db_port}"
    connection_string = f"postgresql+psycopg2://{database_info}/{args.db_passwd}"
    database_engine = create_engine(connection_string)

    tables_to_process = TABLES.copy()
    if args.no_base:
        tables_to_process.remove("base")
    if args.no_rest_state:
        tables_to_process.remove("rest_state")

    recreate_tables(tables_to_process, database_engine)
    populate_tables(tables_to_process, database_engine)
