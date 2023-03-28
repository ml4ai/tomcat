#!/bin/bash

set -u

TIME_DIFFERENCE_SESSION_NAME="time-difference"
TIME_DIFFERENCE_CLIENT_NAME="time-difference-client"

__launch_time_difference_cat_client() {
  local command="cd ~/git/ML4AI/tomcat/human_experiments/lab_software/tomcat-time-difference && "
  command+="python3 run_client.py -n CAT"
  tmux new-session -d -s $TIME_DIFFERENCE_SESSION_NAME -n $TIME_DIFFERENCE_CLIENT_NAME "bash -c '$command'"
}

__launch_time_difference_cat_client
