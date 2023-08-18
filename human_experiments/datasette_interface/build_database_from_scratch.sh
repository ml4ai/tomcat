#!/usr/bin/env bash

# Build database from scratch

echo "Building database from scratch."

#./build_base_tables.py \
#  && ./process_rest_state_task_data.py \
#  && ./process_affective_task_data.py \
#  && ./process_fingertapping_task_data.py \
#  && ./process_ping_pong_competitive_data.py \
#  && ./process_ping_pong_cooperative_data.py \
#  && ./process_testbed_messages.py \
#  && ./process_fnirs_data.py \
#  && ./process_eeg_data.py \
#  && ./update_inspect_file.sh

python build_base_tables.py
python process_rest_state_task_data.py
python process_affective_task_data.py
python process_fingertapping_task_data.py
python process_ping_pong_competitive_data.py
python process_ping_pong_cooperative_data.py
python process_testbed_messages.py
python process_fnirs_data.py
python process_eeg_data.py
python update_inspect_file.sh

echo "Finished building database from scratch"
