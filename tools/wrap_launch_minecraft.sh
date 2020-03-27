#!/bin/bash

# We cannot call this directly because we need TOMCAT and TOMCAT_TMP_DIR. If we
# want to do this, then we need to arrange for setting those vars. 

set -e -u 

minecraft_log="${TOMCAT_TMP_DIR}/minecraft.log"

cd ${TOMCAT}/external/malmo/Minecraft
echo "Running: launchClient.sh in ${TOMCAT}/external/malmo/Minecraft" 
./launchClient.sh < /dev/null >& ${minecraft_log} &
bg_pid=$!
echo $bg_pid > ${TOMCAT_TMP_DIR}/minecraft_launch.pid
echo "Minecraft is starting up in the background."  

wait $bg_pid > /dev/null

launch_status=$?

# In theory, we should have the logical status here, but launchClient
# does not necessarly return failure, and making it do the right thing is
# tricky. 


if [[ $launch_status -eq 0 ]]; then
    have_problem=`grep -c 'BUILD FAILED' ${minecraft_log}`
    if [[ ${have_problem} -ne 0 ]]; then 
        launch_status=1
    fi
fi

if [[ $launch_status -ne 0 ]]; then
    echo " " >> ${minecraft_log}
    echo "launchClient FAILED" >> ${minecraft_log}
else 
    echo "Wait on launchClient process is done."  
    /bin/rm -f ${minecraft_log}
fi 

exit ${launch_status}




