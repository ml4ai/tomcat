#!/bin/bash

# Script to download Minecraft worlds for ToMCAT

set -e

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
