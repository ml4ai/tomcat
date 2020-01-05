#!/bin/bash

# This script is meant to update ToMCAT to the latest version. It is meant to
# be used after running 'git pull', on the master branch. It downloads the
# latest tomcat mission worlds and rebuilds tomcat
#
# Usage: git pull && ./tools/update_tomcat.sh

./tools/download_tomcat_worlds.sh

mkdir -p build

parent_dir=`pwd`

pushd build
    # Kobus says: The above mkdir build suggests that build might not have
    # existed, which means that this will break. Also, if you are uipdating, it
    # might be best to rebuild makefiles. So, I am adding a cmake command. 
    #
    cmake "${parent_dir}"

    make -j
    make -j Minecraft
popd

