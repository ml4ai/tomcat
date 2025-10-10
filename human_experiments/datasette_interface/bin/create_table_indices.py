#!/usr/bin/env python

from datasette_interface.database.entity.signal.eeg import EEGRaw
from datasette_interface.database.entity.signal.fnirs import FNIRSRaw
from datasette_interface.database.entity.signal.gaze import GAZERaw
from datasette_interface.raw.common.process_raw_signals import create_indices

create_indices(EEGRaw, "eeg")
create_indices(FNIRSRaw, "fnirs_tmp")
create_indices(GAZERaw, "gaze")
