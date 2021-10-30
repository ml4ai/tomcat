#!/bin/bash

minecraft_log="${TOMCAT_TMP_DIR}/minecraft.log"

exit_status=0

while [ ! -f $minecraft_log ]; do
  sleep 1;
done

# Once it exists, grab the port when it is printed to the file
if [ ! -f $minecraft_log ]; then
    echo "heyyyyyy--------------------> exiting because there is no log file to grab a port from.";
    exit_status=1
fi
# Grab the tail -f output of the log file and pipe it into
# the subshell generated
if [ $exit_status -eq 0 ]; then
  tail -f $minecraft_log |
    {
      #In the subshell, read the line
      while read cur_line; do

        # Use sed to match the pattern and remove it, leaving
        # only the port id. Print the output to the variable
        # only when the pattern is matched
        lan_port=$(sed -n -e 's/^.*\[Server\ thread\/INFO\]:\ Started\ on\ //p' <<< "$cur_line");
        # If the port id non-empty, then we are done.
        # So, message is published to the topic lan_port_id and exit
        if [ -n "$lan_port" ]; then
          echo "{\"portid\": \"$lan_port\"}" | mosquitto_pub -t lan_port_id -l
          break
        fi

        # Exit if there is no file at any point
        if [ ! -f $log_file ]; then
          echo "Exiting because there is no log file to grab a port from"
          exit_status=1
          break
        fi

      done

  }
fi

exit ${exit_status}

