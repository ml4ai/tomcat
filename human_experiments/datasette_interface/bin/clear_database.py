#!/usr/bin/env python

from datasette_interface.database.config import engine
from datasette_interface.database.entity.base.base import Base

answer = input(
    "[WARN] This operation will erase all tables and data saved on them. Do you want "
    "to proceed? (y/n): "
)
if answer.lower() in ["y", "yes"]:
    Base.metadata.drop_all(engine, checkfirst=True)
    print("Tables dropped successfully.")
else:
    print("Operation aborted.")
