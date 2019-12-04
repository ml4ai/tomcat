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

${TOMCAT}/tools/download_tomcat_data.sh
if [[ $? -ne 0 ]]; then exit 1; fi;


pushd "${TOMCAT}"
    echo "Installing ToMCAT in `pwd`"

    mkdir -p build 
    if [[ $? -ne 0 ]]; then exit 1; fi;

    pushd build > /dev/null 

    if [[ ! -z $TRAVIS ]]; then
      cmake ${TOMCAT}
    else
      # On Travis, we will build HTML documentation by default.
      cmake ${TOMCAT} -DBUILD_DOCS=ON
    fi;

    if [[ $? -ne 0 ]]; then exit 1; fi;

    make -j
    if [[ $? -ne 0 ]]; then exit 1; fi;

    make -j Minecraft
    if [[ $? -ne 0 ]]; then exit 1; fi;
popd > /dev/null 

mkdir bin
if [[ $? -ne 0 ]]; then exit 1; fi;

echo " "
echo "Finished installing ToMCAT in ${TOMCAT}!"
echo " "
