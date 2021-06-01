#!/bin/bash


log_file="/tmp/$USER/tomcat/minecraft.log";

# Wait till the log file actually exists
echo "Looking for log file...";

while [ ! -f $log_file ]; do
  sleep 1;
done

# Once it exists, grab the port when it is printed to the file
echo 'Log file found. Monitoring for port...';

# Grab the tail -f output of the log file and pipe it into
# the subshell generated
tail -f $log_file |
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
        echo $lan_port | mosquitto_pub -t lan_port_id -l
        exit 0;
      fi

      # Exit if there is no file at any point
      if [ ! -f $log_file ]; then
        echo "Exiting because there is no log file to grab a port from.";
        exit 0;
      fi

    done

}

