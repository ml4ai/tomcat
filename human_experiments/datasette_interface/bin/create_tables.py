#!/usr/bin/env python

from datasette_interface.database.config import Base, engine

Base.metadata.create_all(engine, checkfirst=True)
