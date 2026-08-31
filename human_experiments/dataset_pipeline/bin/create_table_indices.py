#!/usr/bin/env python

from dataset_pipeline.database.entity.signal.eeg import EEGRaw
from dataset_pipeline.database.entity.signal.fnirs import FNIRSRaw
from dataset_pipeline.database.entity.signal.gaze import GAZERaw
from dataset_pipeline.raw.common.process_raw_signals import create_indices

create_indices(EEGRaw, "eeg")
create_indices(FNIRSRaw, "fnirs")
create_indices(GAZERaw, "gaze")
