#!/bin/bash

# We cannot use '-u' because we need to be able to test if TRAVIS is set. We
# cannot simply declare TRAVIS and then test if it is NULL, as cmake checks
# whether TRAVIS is set. If we had confidence that tcsh was installed, then we
# could use tcsh to check if the variable was bound, but this is an install
# script that is supposed to install things we might need, counting on as little
# as possible.

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
export TOMCAT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" >/dev/null 2>&1 && pwd )"

###############################################################################

# On Travis, we will create and activate a Python virtual environment
if [[ ! -z $TRAVIS ]]; then
  if [[ "$TRAVIS_OS_NAME" == "osx" ]]; then
    echo "Creating virtual environment for tomcat"
    brew unlink python@2
    brew link --overwrite python
    if [[ $? -ne 0 ]]; then exit 1; fi;
  else
    sudo apt-get install python3-venv
  fi

  python3 -m venv tomcat_venv
  if [[ $? -ne 0 ]]; then exit 1; fi;

  echo "Activating virtual environment for tomcat"
  source tomcat_venv/bin/activate
  # Installing Sphinx HTML documentation requirements.
  pip install exhale recommonmark sphinx-rtd-theme
  if [[ $? -ne 0 ]]; then exit 1; fi;
fi

${TOMCAT}/tools/install_dependencies.sh
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
        # appropriate version of Java on their macOS image.
        if [[ -z $TRAVIS ]]; then
            make -j Minecraft
            if [[ $? -ne 0 ]]; then exit 1; fi;
        fi
    popd > /dev/null 
popd > /dev/null 

${TOMCAT}/tools/download_tomcat_worlds.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

${TOMCAT}/tools/download_OpenFace_models.sh
if [[ $? -ne 0 ]]; then exit 1; fi;

echo " "
echo "Finished installing ToMCAT in ${TOMCAT}!"
echo " "
exit 0
