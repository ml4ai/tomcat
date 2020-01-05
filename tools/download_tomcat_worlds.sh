#!/bin/bash

# Script to download Minecraft worlds for ToMCAT

# Kobus says 'set -e' does not make sense if you are checking returns as you
# are doing.
#
# set -e

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

echo "Downloading ToMCAT Minecraft worlds."

pushd ${TOMCAT}/data > /dev/null 
    # Download and extract the manually-created worlds from vanga.
    curl -O http://vanga.sista.arizona.edu/tomcat/data/worlds.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
    tar -xvzf worlds.tgz && rm -rf worlds.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
popd > /dev/null

echo "ToMCAT worlds download complete."
echo " "
