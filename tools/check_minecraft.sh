#!/bin/bash

set -u 

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

export TOMCAT_TMP_DIR="/tmp/$USER/tomcat"
mkdir -p "${TOMCAT_TMP_DIR}"
if [[ $? -ne 0 ]]; then exit 1; fi;
 
minecraft_log="${TOMCAT_TMP_DIR}/minecraft.log"
minecraft_launch_pid_file="${TOMCAT_TMP_DIR}/minecraft_launch.pid"

exit_status=1
num_tries=2

# Likely a bit more robust and easier to debug if we give Minecraft some time to
# start. However, this is optional.
#
initial_wait=10 

num_seconds_to_wait=300

try=0
while [ $try -lt $num_tries ]; do 
    echo " "

    must_launch=0

    if [[ -e "${minecraft_launch_pid_file}" ]]; then 
        minecraft_launch_pid=`cat "${minecraft_launch_pid_file}"` 
        ps ${minecraft_launch_pid} >& /dev/null 

        if [[ $? -ne 0 ]]; then
            echo "Minecraft launch process is no longer around." 
            must_launch=1;
        else 
            echo "We found an existing Minecraft launch process." 
        fi;
    fi 

    if [[ ${must_launch} -eq 0 ]]; then 
        if [[ ! -e "${minecraft_log}" ]]; then 
            echo "We cannot find the Minecraft launch process log." 
            must_launch=1;
        else 
            echo "We found the corresponding Minecraft launch process log." 
        fi 
    fi 

    if [[ ${must_launch} -ne 0 ]]; then 
        echo "Script check_minecraft executing: "
        echo "    ${TOMCAT}/tools/wrap_launch_minecraft.sh" 
        ${TOMCAT}/tools/wrap_launch_minecraft.sh & 
        echo "Waiting for ${initial_wait} seconds as Minecraft takes at least that long to get ready."  
        sleep ${initial_wait}
    fi 

    if [[ ! -e "${minecraft_log}" ]]; then 
        echo "${minecraft_log} does not exist."
        echo "We expected ${TOMCAT}/tools/wrap_launch_minecraft.sh to have created it."
    else 
        have_line=0
        num_seconds=0

        echo "Now we will repeatedly check the log file until Minecraft looks ready."

        while [ $num_seconds -lt $num_seconds_to_wait ]; do 
            if [[ -e "${minecraft_log}" ]]; then 
                have_line=`grep -c 'CLIENT enter state: DORMANT' ${minecraft_log}`
                if [[ ${have_line} -ne 0 ]]; then break; fi
            else 
                echo "${minecraft_log} has disappeared."
                break
            fi 

            sleep 1

            let num_seconds+=1

            echo -e -n "\r${num_seconds}/${num_seconds_to_wait}"
        done 

        echo " "

        if [ $have_line -gt 0 ]; then
            echo "Minecraft seems ready for us."
            echo "If it does not work, then run:"
            echo "    ${TOMCAT}/tools/kill_minecraft.sh"
            echo "and try again." 
            exit_status=0
            break
        elif [[ $num_seconds -eq $num_seconds_to_wait ]]; then
            echo "Checking Minecraft timed out after ${num_seconds_to_wait}."
        fi
    fi 

    echo "Script check_minecraft.sh is having trouble verifying a viable Minecraft client."

    let try+=1 

    if [[ $try -lt $num_tries ]]; then 
        echo "Killing all Minecraft and Malmo processes that can be found and trying again."
    fi 

    ${TOMCAT}/tools/kill_minecraft.sh

    sleep 1
    /bin/rm -f "${TOMCAT_TMP_DIR}/minecraft.log"
    sleep 1
done

echo " "

if [[ ${exit_status} -ne 0 ]]; then
    echo "Giving up checking Minecraft after ${num_tries} tries."
fi 

echo "Finished checking Minecraft with with exit_status ${exit_status}."
echo " "

exit ${exit_status}

