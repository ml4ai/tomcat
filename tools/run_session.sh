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

# ${TOMCAT}/tools/install.sh

${TOMCAT}/tools/check_minecraft.sh

# tmp_dir="/tmp/$USER/tomcat"
# mkdir -p "${tmp_dir}"

echo " "
echo "Now running the tutorial mission in ${TOMCAT}."
echo " "

${TOMCAT}/build/bin/runExperiment --mission 0

echo "Tutorial mission ended with exits status $?"

echo " "
echo "Now running the Zombie invasion mission in ${TOMCAT}."
echo " "

${TOMCAT}/build/bin/runExperiment --mission 1 --time_limit 600 

echo "Zombie invasion mission ended with exits status $?"

echo " "
echo "Finished running all sessions in ${TOMCAT}."
echo " "


