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

${TOMCAT}/tools/install_dependencies.sh
if [[ $? -ne 0 ]]; then exit 1; fi;


pushd "${TOMCAT}"
    echo "Building ToMCAT in `pwd`"

    /bin/rm -rf build
    mkdir build
    if [[ $? -ne 0 ]]; then exit 1; fi;

    # Trying to set the correct version of Java.
    macports_found=`[ -x "$(command -v port)" ]; echo $?`
    if [[ $macports_found -eq 1 ]]; then
      export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk8/Contents/Home
    fi
    pushd build > /dev/null 
        if [[ -n $GITHUB_ACTIONS ]]; then
            cmake ${TOMCAT} -DBoost_ARCHITECTURE=-x64\
                            -DBOOST_ROOT=$BOOST_ROOT_1_69_0
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
