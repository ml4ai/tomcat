#!/bin/bash

# This script is meant to update ToMCAT to the latest version. It is meant to
# be used after running 'git pull', on the master branch. It downloads the
# latest tomcat mission worlds and rebuilds tomcat
#
# Usage: ./tools/update_tomcat.sh

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
TOMCAT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" >/dev/null 2>&1 && pwd )"
export TOMCAT

pushd ${TOMCAT}
  git pull
  ${TOMCAT}/tools/install.sh
popd
