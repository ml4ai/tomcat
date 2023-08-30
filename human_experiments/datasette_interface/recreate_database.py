#!/usr/bin/env python

import argparse
from sqlalchemy import create_engine
import logging
from logging import info
import sys

from config import USER
from entity.base.base import Base

from build_base_tables import build_base_tables
from process_rest_state_task_data import process_rest_state_task_data

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

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="""
            Builds a PostgresSQL database from scratch. All the tables in the database will be dropped, 
            recreated and repopulated after running this script. It assumes the cluster and database was previously
            created. The script will not work if this is not true.           
        """
    )

    parser.add_argument("--db_user", type=str, required=False, default=USER,
                        help="User with granted writing permissions in the database.")
    parser.add_argument("--db_port", type=int, required=True,
                        help="Port where the cluster is running. Make sure to set the port to your local cluster.")
    parser.add_argument("--db_name", type=str, required=False, default=f"{USER}_tomcat",
                        help="Database name. Make sure the database was previously created before running this script.")
    parser.add_argument("--db_passwd", type=str, required=True, help="Password to connect to the database.")
    parser.add_argument("--no_base", type=str, required=True, help="Password to connect to the database.")

    args = parser.parse_args()

    database_info = f"{args.db_user}:{args.db_name}@localhost:{args.db_port}"
    connection_string = f"postgresql+psycopg2://{database_info}/{args.db_passwd}"
    database_engine = create_engine(connection_string)

    info(f"Dropping all tables from {database_info}.")
    Base.metadata.drop_all(database_engine, checkfirst=True)

    info(f"Recreating all tables in {database_info}.")
    Base.metadata.create_all(database_engine)

    build_base_tables(database_engine)
    process_rest_state_task_data(database_engine)
