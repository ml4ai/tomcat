#!/bin/bash

# We cannot use '-u' because we need to be able to test if TRAVIS is set. We
# cannot simply declare TRAVIS and then test if it is NULL, as cmake checks
# whether TRAVIS is set. If we had confidence that tcsh was installed, then we
# could use tcsh to check if the variable was bound, but this is an install
# script that is supposed to install things we might need, counting on as little
# as possible.

# set -x 

###############################################################################

# If TOMCAT is set as an enviroment variable, we will respect it. We do not use
# the tcsh trick to test this because tcsh might not be installed yet. 

if [ ! -z "$TOMCAT" ]; then
    echo "Using requested TOMCAT location ${TOMCAT}."
else 
    # This script should be in a directory 'tools' which should be a subdirectory of
    # the TOMCAT directory. The following uses those assumptions to determine
    # TOMCAT.
    #
    called_as_dir=`echo $0 | sed 's#^[^/][^/]*$#./#'`
    called_as_dir=`echo $called_as_dir | sed 's#^\(.*\)/.*$#\1#'`
    pushd "${called_as_dir}" > /dev/null; called_as_dir=`pwd`; popd > /dev/null
    export TOMCAT=`echo $called_as_dir | sed 's#^\./##' | sed 's#^\(.*\)/tools$#\1#'`
    echo "Using inferreed TOMCAT location ${TOMCAT}."
fi

###############################################################################

# Bug! Interacts badly with cmake!! 
# declare -x TRAVIS

${TOMCAT}/tools/install_dependencies.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

# On Travis, we will create and activate a Python virtual environment
if [[ ! -z $TRAVIS ]]; then
  if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    echo "Creating virtual environment for tomcat"
    brew unlink python@2
    brew link --overwrite python
    python3 -m venv tomcat_venv
    if [[ $? -ne 0 ]]; then exit 1; fi;

    echo "Activating virtual environment for tomcat"
    source tomcat_venv/bin/activate
    if [[ $? -ne 0 ]]; then exit 1; fi;

    # Installing Sphinx HTML documentation requirements.
    pip install exhale recommonmark sphinx-rtd-theme
    if [[ $? -ne 0 ]]; then exit 1; fi;
  fi
fi

${TOMCAT}/tools/download_tomcat_worlds.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

${TOMCAT}/tools/download_OpenFace_models.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

pushd "${TOMCAT}"
    echo "Building ToMCAT in `pwd`"

    mkdir -p build 
    if [[ $? -ne 0 ]]; then exit 1; fi;

    pushd build > /dev/null 
        if [[ ! -z $TRAVIS ]]; then
            # On Travis, we will build HTML documentation by default.
            cmake ${TOMCAT} -DBUILD_DOCS=ON
            if [[ $? -ne 0 ]]; then exit 1; fi;
        else
            cmake ${TOMCAT}
            if [[ $? -ne 0 ]]; then exit 1; fi;
        fi;

        make -j
        if [[ $? -ne 0 ]]; then exit 1; fi;

        # We skip building Minecraft on Travis since we cannot get an
        # appropriate version of Java on their MacOS image.
        if [[ -z $TRAVIS ]]; then
            make -j Minecraft
            if [[ $? -ne 0 ]]; then exit 1; fi;
        fi
    popd > /dev/null 
popd > /dev/null 


echo " "
echo "Finished installing ToMCAT in ${TOMCAT}!"
echo " "

