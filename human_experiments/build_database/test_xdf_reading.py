#!/usr/bin/env python

import pyxdf
import json

xdf_path = "/tomcat/data/raw/LangLab/experiments/study_3_pilot/group/exp_2022_10_04_09/lion/eeg_fnirs_pupil/lion_eeg_fnirs_pupil.xdf"

data, header = pyxdf.load_xdf(xdf_path)
for stream in data:
    print(json.dumps(stream["info"]))
