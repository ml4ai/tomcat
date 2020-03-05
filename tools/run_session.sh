#!/bin/bash

set -u 

mission_one_time=60
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
    # the TOMCAT environment variable.
    #
    called_as_dir=`echo $0 | sed 's#^[^/][^/]*$#./#'`
    called_as_dir=`echo $called_as_dir | sed 's#^\(.*\)/.*$#\1#'`
    pushd "${called_as_dir}" > /dev/null; called_as_dir=`pwd`; popd > /dev/null
    export TOMCAT=`echo $called_as_dir | sed 's#^\./##' | sed 's#^\(.*\)/tools$#\1#'`
    echo "Script check_minecraft is using inferred TOMCAT location ${TOMCAT}."
fi

###############################################################################

${TOMCAT}/tools/check_minecraft.sh

export TOMCAT_TMP_DIR="/tmp/$USER/tomcat"
mkdir -p "${TOMCAT_TMP_DIR}"
if [[ $? -ne 0 ]]; then exit 1; fi;

tutorial_mission_log="${TOMCAT_TMP_DIR}/tutorial_mission.log"
zombie_invasion_log="${TOMCAT_TMP_DIR}/zombie_invasion.log"

num_tries=2

if [[ ${do_tutorial} -eq 1 ]]; then
    try=0
    while [ $try -lt $num_tries ]; do 
        echo " "
        echo "Running the tutorial mission in ${TOMCAT}."
        echo " "

        ${TOMCAT}/build/bin/runExperiment --mission 0 >& ${tutorial_mission_log} &
        bg_pid=$!
        echo "Running: ${TOMCAT}/build/bin/runExperiment --mission 0"
        echo "Process is $bg_pid" 
        echo "Waiting for it"
        wait
        tutorial_mission_status=$?

        if [[ ${tutorial_mission_status} -eq 0 ]]; then
            tutorial_mission_status=`grep -c 'Error starting mission' ${tutorial_mission_log}`
        fi


        if [[ ${tutorial_mission_status} -eq 0 ]]; then
            echo "Tutorial mission ended with success status."
            echo " "
            break
        fi 

        let try+=1 

        if [[ $try -lt $num_tries ]]; then 
            echo "Tutorial mission ended with failure status."
            echo "Killing all Minecraft and Malmo processes that can be found and trying again."
            ${TOMCAT}/tools/kill_minecraft.sh
            ${TOMCAT}/tools/check_minecraft.sh
        fi 
    done
fi

if [[ ${do_invasion} -eq 1 ]]; then
    echo " "
    echo "Running the Zombie invasion mission in ${TOMCAT}."
    echo " "

    #Recording players face through WebCam.
    ffmpeg -nostdin -f avfoundation -i "default" webcam_video.mpg >&/dev/null &
    webcam_recording_pid=$!

    #Recording game screen.

    ffmpeg -nostdin -f avfoundation -i "1:none" screen_video.mpg &>/dev/null &
    screen_recording_pid=$!

    try=0
    while [ $try -lt $num_tries ]; do 
        ${TOMCAT}/build/bin/runExperiment --mission 1 --time_limit ${mission_one_time}>& ${zombie_invasion_log} &
        bg_pid=$!
        echo "Running: ${TOMCAT}/build/bin/runExperiment --mission 1 --time_limit ${mission_one_time}"
        echo "Process is $bg_pid" 
        echo "Waiting for it"
        echo " "
        wait
        zombie_invasion_status=$?

        if [[ ${zombie_invasion_status} -eq 0 ]]; then
            zombie_invasion_status=`grep -c 'Error starting mission' ${zombie_invasion_log}`
        fi

        if [[ ${zombie_invasion_status} -eq 0 ]]; then
            echo "Zombie invasion mission ended with success status."
            echo " "
            break
        fi 

        let try+=1 

        if [[ $try -lt $num_tries ]]; then 
            echo "Zombie invasion mission ended with failure status."
            echo "Killing all Minecraft and Malmo processes that can be found and trying again."
            ${TOMCAT}/tools/kill_minecraft.sh
            ${TOMCAT}/tools/check_minecraft.sh
        fi 
        kill -2 $webcam_recording_pid
        kill -2 $screen_recording_pid
    done
fi

echo "Finished running all sessions in ${TOMCAT}."
echo " "
