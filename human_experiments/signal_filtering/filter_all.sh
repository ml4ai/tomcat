#!/bin/bash

echo "Filtering fNIRS..."
python filter_fnirs.py

echo "Filtering EEG..."
python filter_eeg.py

echo "Filtering EKG..."
python filter_ekg.py

echo "Filtering GSR..."
python filter_gsr.py
