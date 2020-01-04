#!/bin/bash

# This script is meant to update ToMCAT to the latest version. It is meant to
# be used after running 'git pull', on the master branch. It downloads the
# latest tomcat mission worlds and rebuilds tomcat
#
# Usage: git pull && ./tools/update_tomcat.sh

./tools/download_tomcat_worlds.sh

mkdir -p build

pushd build
  make -j
  make -j Minecraft
popd
