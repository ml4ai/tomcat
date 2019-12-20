#!/bin/bash

# Script to download OpenFace models for ToMCAT

set -e

echo "Downloading OpenFace models."

pushd ${TOMCAT}/data > /dev/null 
    # Download and extract the OpenFace models
    curl -O http://vision.cs.arizona.edu/adarsh/OpenFace_models.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
    tar -xvzf OpenFace_models.tgz && rm -rf OpenFace_models.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
popd > /dev/null

echo "OpenFace models download complete."
echo " "
