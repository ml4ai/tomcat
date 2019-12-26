#!/bin/bash

# We cannot call this directly because we need TOMCAT and TOMCAT_TMP_DIR. If we
# want to do this, then we need to arrange for setting those vars. 

set -e -u 

cd ${TOMCAT}/external/malmo/Minecraft
echo "Running: launchClient.sh in ${TOMCAT}/external/malmo/Minecraft" 
./launchClient.sh < /dev/null >& ${TOMCAT_TMP_DIR}/minecraft.log &
bg_pid=$!
echo $bg_pid > ${TOMCAT_TMP_DIR}/minecraft_launch.pid
echo "Minecraft is starting up in the background."  

wait >& /dev/null

# cat ${TOMCAT_TMP_DIR}/minecraft.log 
/bin/rm -f ${TOMCAT_TMP_DIR}/minecraft.log 


