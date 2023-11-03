#!/usr/bin/env python

"""This script queries the ToMCAT SQLite3 database to get the group_session is
and creates directories to hold the corresponding screenshots. This is meant to
be run on the ivilab server."""

import sqlite3
from entity.signal.screen_capture import ScreenCapture
from sqlalchemy import create_engine, select
from sqlalchemy.orm import Session

database_engine = create_engine("sqlite:////var/www/data/tomcat/tomcat.db")
with Session(database_engine.connect()) as database_session:
    statement = select(ScreenCapture)
    objects = database_session.scalars(statement).fetchmany(10)
    print(objects)
