#!/bin/bash

set -u 

mission_one_time=120
do_tutorial=0
do_invasion=1

###############################################################################

# If TOMCAT is set as an enviroment variable, we will respect it. 

declare -x TOMCAT
if [ ! -z "$TOMCAT" ]; then
    echo "Script check_minecraft is using requested TOMCAT location ${TOMCAT}."
else 
    # This script should be in a directory 'tools' which should be a subdirectory of
    # the TOMCAT directory. The following uses those assumptions to determine
    # TOMCAT.
    #
    called_as_dir=`echo $0 | sed 's#^[^/][^/]*$#./#'`
    called_as_dir=`echo $called_as_dir | sed 's#^\(.*\)/.*$#\1#'`
    pushd "${called_as_dir}" > /dev/null; called_as_dir=`pwd`; popd > /dev/null
    export TOMCAT=`echo $called_as_dir | sed 's#^\./##' | sed 's#^\(.*\)/tools$#\1#'`
    echo "Script check_minecraft is using inferreed TOMCAT location ${TOMCAT}."
fi

###############################################################################

# ${TOMCAT}/tools/install.sh

${TOMCAT}/tools/check_minecraft.sh

if [[ ${do_tutorial} -eq 1 ]]; then
    echo " "
    echo "Running the tutorial mission in ${TOMCAT}."
    echo " "

    echo "About to run: ${TOMCAT}/build/bin/runExperiment --mission 0"
    echo "Required env var TOMCAT is set to: ${TOMCAT}" 

    ${TOMCAT}/build/bin/runExperiment --mission 0 &
    bg_pid=$!
    echo "Running: ${TOMCAT}/build/bin/runExperiment --mission 0"
    echo "Process is $bg_pid" 
    echo "Waiting for it"
    wait

    echo "Tutorial mission ended with exits status $?"
    echo " "
fi

if [[ ${do_invasion} -eq 1 ]]; then
    echo "Running the Zombie invasion mission in ${TOMCAT}."

    ${TOMCAT}/build/bin/runExperiment --mission 1 --time_limit ${mission_one_time} &
    bg_pid=$!
    echo "Running: ${TOMCAT}/build/bin/runExperiment --mission 1 --time_limit ${mission_one_time}"
    echo "Process is $bg_pid" 
    echo "Waiting for it"
    echo " "
    wait

    echo "Zombie invasion mission ended with exits status $?"
    echo " "
fi

echo "Finished running all sessions in ${TOMCAT}."
echo " "


