#!/bin/bash

# Script to download OpenFace models for ToMCAT

set -e

echo "Downloading OpenFace models."

TOMCAT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../../" >/dev/null 2>&1 && pwd )"
export TOMCAT

pushd ${TOMCAT}/data > /dev/null 
    # Download and extract the OpenFace models
    curl -O http://vision.cs.arizona.edu/adarsh/OpenFace_models.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
    tar -xvf OpenFace_models.tgz && rm -rf OpenFace_models.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
popd > /dev/null

echo "OpenFace models download complete."
echo " "
exit 0
