#!/usr/bin/env python

from datasette_interface.database.config import engine
from datasette_interface.database.config import Base

Base.metadata.create_all(engine, checkfirst=True)
