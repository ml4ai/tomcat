#!/usr/bin/env bash

# Build database from scratch

./process_rick_workbook.py
#./process_rest_state_task_data.py
#./process_fnirs_data.py
./process_affective_task_data.py
./process_testbed_messages.py
