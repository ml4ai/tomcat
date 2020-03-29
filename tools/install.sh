#!/bin/bash

# We cannot use '-u' because we need to be able to test if GITHUB_ACTIONS is set. We
# cannot simply declare GITHUB_ACTIONS and then test if it is NULL, as cmake checks
# whether GITHUB_ACTIONS is set. If we had confidence that tcsh was installed, then we
# could use tcsh to check if the variable was bound, but this is an install
# script that is supposed to install things we might need, counting on as little
# as possible.

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
TOMCAT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" >/dev/null 2>&1 && pwd )"
export TOMCAT

###############################################################################

# On Github Actions, we will create and activate a Python virtual environment
if [[ -n $GITHUB_ACTIONS ]]; then
  if [ -x "$(command -v apt-get)" ]; then
    sudo apt-get update
    sudo apt-get install -y python3-venv xvfb
    python3 -m venv tomcat_venv
    source tomcat_venv/bin/activate
    pip install exhale recommonmark sphinx-rtd-theme
  fi
fi


${TOMCAT}/tools/install_dependencies.sh
if [[ $? -ne 0 ]]; then exit 1; fi;


pushd "${TOMCAT}"
    echo "Building ToMCAT in `pwd`"

    mkdir -p build 
    if [[ $? -ne 0 ]]; then exit 1; fi;

    pushd build > /dev/null 
        if [[ ! -z $GITHUB_ACTIONS ]]; then
            cmake ${TOMCAT} -DBoost_ARCHITECTURE=-x64\
                            -DBOOST_ROOT=$BOOST_ROOT_1_69_0\
                            -DBUILD_DOCS=ON
            if [[ $? -ne 0 ]]; then exit 1; fi;
        else
            cmake ${TOMCAT}
            if [[ $? -ne 0 ]]; then exit 1; fi;
        fi;

        make -j
        if [[ $? -ne 0 ]]; then exit 1; fi;

        make -j Minecraft
    popd > /dev/null 
popd > /dev/null 

${TOMCAT}/tools/download_tomcat_worlds.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

#${TOMCAT}/tools/download_OpenFace_models.sh
#if [[ $? -ne 0 ]]; then exit 1; fi;

echo " "
echo "Finished installing ToMCAT in ${TOMCAT}!"
echo " "
exit 0
