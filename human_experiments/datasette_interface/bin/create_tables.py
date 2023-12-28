#!/usr/bin/env python

from datasette_interface.database.config import Base, engine  # noqa f401
from datasette_interface.database.entity.base.task import Task  # noqa f401
from datasette_interface.database.entity.signal.eeg import EEGRaw  # noqa f401
from datasette_interface.database.entity.task.minecraft_task import (  # noqa f401
    MinecraftMission, MinecraftTestbedMessage)

Base.metadata.create_all(engine, checkfirst=True)
