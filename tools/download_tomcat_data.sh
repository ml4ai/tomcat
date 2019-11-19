#!/bin/bash

# Script to download the data required for ToMCAT

set -e

cd ${TOMCAT}/data

# Download and extract the manually-created worlds from vanga.
curl -O http://vanga.sista.arizona.edu/tomcat/data/worlds.tgz
tar -xvzf worlds.tgz && rm -rf worlds.tgz

# Download and extract the OpenFace models
curl -O http://vision.cs.arizona.edu/adarsh/OpenFace_models.tgz
tar -xvzf OpenFace_models.tgz && rm -rf OpenFace_models.tgz
