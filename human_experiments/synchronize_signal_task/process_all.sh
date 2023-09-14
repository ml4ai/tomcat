#!/bin/bash

python process_fnirs_10hz.py
python process_eeg_500hz.py
python process_ekg_500hz.py
python process_gsr_500hz.py
python process_all_120hz.py
python process_all_1000hz.py
