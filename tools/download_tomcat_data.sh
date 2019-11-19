#!/bin/bash

# Script to download the data required for ToMCAT

set -e

cd ${TOMCAT}/data

# Download and extract the manually-created worlds from vanga.
curl -O http://vanga.sista.arizona.edu/tomcat/data/worlds.tgz
tar -xvzf worlds.tgz
cd ..
