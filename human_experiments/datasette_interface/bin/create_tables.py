#!/usr/bin/env python

from datasette_interface.database.config import Base, engine  # noqa f401
from datasette_interface.database.entity.derived.eeg_sync import \
    EEGSync  # noqa f401
from datasette_interface.database.entity.derived.ekg_sync import \
    EKGSync  # noqa f401
from datasette_interface.database.entity.derived.fnirs_sync import \
    FNIRSSync  # noqa f401
from datasette_interface.database.entity.derived.gsr_sync import \
    GSRSync  # noqa f401
from datasette_interface.database.entity.signal.eeg import EEGRaw  # noqa f401
from datasette_interface.database.entity.signal.fnirs import \
    FNIRSRaw  # noqa f401
from datasette_interface.database.entity.signal.gaze import \
    GAZERaw  # noqa f401
from datasette_interface.database.entity.signal.screen_capture import \
    ScreenCapture  # noqa f401

Base.metadata.create_all(engine, checkfirst=True)
