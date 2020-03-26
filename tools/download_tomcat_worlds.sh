#!/bin/bash

# Script to download Minecraft worlds for ToMCAT

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
export TOMCAT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" >/dev/null 2>&1 && pwd )"

echo "Downloading ToMCAT Minecraft worlds."

pushd ${TOMCAT}/data > /dev/null 
    # Download and extract the manually-created worlds from vanga.
    curl -O http://vanga.sista.arizona.edu/tomcat/data/worlds.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
    tar -xzf worlds.tgz && rm -rf worlds.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
popd > /dev/null

echo "ToMCAT worlds download complete."
echo " "
exit 0
