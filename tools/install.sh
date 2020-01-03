#!/bin/bash

called_as=`echo $0 | sed | sed 's#^\./##'` 
script_path=`pwd`/$called_as 
tomcat=`echo $script_path | sed 's#^\./##' | sed 's#^\(.*\)//*tools/*install.sh#\1#'`

# We do not actually need to consult TOMCAT as an environment variable, but it
# gives us a chance to remind the the user that we are ignoring their location
# hint. 

if [ ! -z "$TOMCAT" ]; then
    if [[ "${TOMCAT}" != "${tomcat}" ]]; then
        echo "Resetting TOMCAT variable from ${TOMCAT} to ${tomcat}."
    fi
fi

export TOMCAT=${tomcat}


${TOMCAT}/tools/install_dependencies.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

${TOMCAT}/tools/download_tomcat_worlds.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

${TOMCAT}/tools/download_OpenFace_models.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

# On Travis, we will create and activate a Python virtual environment
if [[ ! -z $TRAVIS ]]; then
  if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    echo "Creating virtual environment for tomcat"
    /usr/local/bin/python3 -m venv tomcat_venv
    if [[ $? -ne 0 ]]; then exit 1; fi;

    echo "Activating virtual environment for tomcat"
    source tomcat_venv/bin/activate
    if [[ $? -ne 0 ]]; then exit 1; fi;

    # Installing Sphinx HTML documentation requirements.
    pip install exhale recommonmark sphinx-rtd-theme
    if [[ $? -ne 0 ]]; then exit 1; fi;
  fi
fi

pushd "${TOMCAT}"


    echo "Installing ToMCAT in `pwd`"

    mkdir -p build 
    if [[ $? -ne 0 ]]; then exit 1; fi;

    pushd build > /dev/null 

    if [[ ! -z $TRAVIS ]]; then
      # On Travis, we will build HTML documentation by default.
      cmake ${TOMCAT} -DBUILD_DOCS=ON
    else
      cmake ${TOMCAT}
    fi;

    if [[ $? -ne 0 ]]; then exit 1; fi;

    make -j
    if [[ $? -ne 0 ]]; then exit 1; fi;

    # We skip building Minecraft on Travis since we cannot get an
    # appropriate version of Java on their MacOS image.
    if [[ -z $TRAVIS ]]; then
      make -j Minecraft
    fi

    if [[ $? -ne 0 ]]; then exit 1; fi;
popd > /dev/null 

mkdir bin
if [[ $? -ne 0 ]]; then exit 1; fi;

echo " "
echo "Finished installing ToMCAT in ${TOMCAT}!"
echo " "
