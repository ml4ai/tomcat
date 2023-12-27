#!/usr/bin/env python

from datasette_interface.database.entity.signal.eeg import EEGRaw
from datasette_interface.database.entity.signal.fnirs import FNIRSRaw
from datasette_interface.database.entity.signal.gaze import (GazeEye0Raw,
                                                             GazeEye1Raw)
from datasette_interface.raw.common.process_raw_signals import create_indices

create_indices(EEGRaw, "eeg")
create_indices(FNIRSRaw, "fnirs")
create_indices(GazeEye0Raw, "gaze_eye0")
create_indices(GazeEye1Raw, "gaze_eye1")
