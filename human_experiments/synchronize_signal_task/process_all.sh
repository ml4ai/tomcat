#!/bin/bash

echo "Processing fNIRS 10 Hz..."
python process_fnirs_10hz.py

echo "Processing EEG 500 Hz..."
python process_eeg_500hz.py

echo "Processing EKG 500 Hz..."
python process_ekg_500hz.py

echo "Processing GSR 500 Hz..."
python process_gsr_500hz.py

echo "Processing all signals 120 Hz..."
python process_all_120hz.py

echo "Processing all signals 1000 Hz..."
python process_all_1000hz.py
