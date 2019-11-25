#!/bin/bash

# Script to download the data required for ToMCAT

set -e

echo "Downloading ToMCAT data."

pushd ${TOMCAT}/data > /dev/null 
    # Download and extract the manually-created worlds from vanga.
    curl -O http://vanga.sista.arizona.edu/tomcat/data/worlds.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
    tar -xvzf worlds.tgz && rm -rf worlds.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;

    # Download and extract the OpenFace models
    curl -O http://vision.cs.arizona.edu/adarsh/OpenFace_models.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
    tar -xvzf OpenFace_models.tgz && rm -rf OpenFace_models.tgz
    if [[ $? -ne 0 ]]; then exit 1; fi;
popd > /dev/null

echo "ToMCAT data download complete."
echo " "
