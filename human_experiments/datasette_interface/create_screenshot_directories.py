#!/usr/bin/env python

"""This script queries the ToMCAT SQLite3 database to get the group_session is
and creates directories to hold the corresponding screenshots. This is meant to
be run on the ivilab server."""

import os
import sqlite3
from tqdm import tqdm

with sqlite3.connect("/var/www/data/tomcat/tomcat.db") as connection:
    group_sessions = [
        x[0]
        for x in connection.execute("SELECT id from group_session;").fetchall()
    ]
    for session in tqdm(group_sessions):
        for station in ["lion", "tiger", "leopard", "cheetah"]:
            os.makedirs(
                f"/var/www/data/tomcat/screenshots/{session}/{station}",
                exist_ok=True
            )
