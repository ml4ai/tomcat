# This bash file needs to be sourced because it is meant to change the caller's
# enviroment. All "local" variables are prefixed by "tmp_".

# Generic error reporting for basic commands
trap 'exit_status=$?;
      echo 
      echo TRP: Command on or near line $LINENO of $0 failed with status $exit_status 
      #
      # Record time in case we need to map issues to system logs. 
      #
      echo TRP: Time $(date)
      #
      echo RAW: $BASH_COMMAND
      #
      # The following naive parsing and eval assumes a limited number of cases
      # of commands, all of which are in code that uses this file, and thus
      # should be tested.
      # 
      tmp_clean_cmd=$(echo ${BASH_COMMAND} | sed "s/2>.*//")
      # echo CMD: ${tmp_clean_cmd} 1>&2;
      #
      # Extract command from a=$(b).`
      tmp_clean_cmd=$(echo ${tmp_clean_cmd} | sed "s/.*=\$(\(.*\))$/\1/")
      #
      eval echo EXP: ${tmp_clean_cmd}
      echo ' ERR

# Since this is sourced, we should unset temporary variables
unset tmp_clean_cmd
