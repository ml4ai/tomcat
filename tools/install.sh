#!/bin/bash

set -e 

./tools/install_dependencies.sh
./tools/download_tomcat_data.sh

echo "Installing ToMCAT..."

mkdir build 
cd build 
cmake ..
make -j
cd ../

echo "Setting TOMCAT environment variable automatically..."

if [ -z "$TOMCAT" ]; then

  if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "MacOS detected. Assuming that you are using bash as your default shell."
    echo "If this is not the case, then make sure to set the TOMCAT"\
    "environment variable to point to the tomcat/ directory."
    echo ""
    echo "# The following line has been automatically added to"\
    "your ~/.bash_profile file:"
    echo ""
    echo "export TOMCAT=`pwd`"
    echo "# TOMCAT environment variable automatically set by TOMCAT installer." >> ~/.bash_profile
    echo "export TOMCAT=`pwd`" >> ~/.bash_profile
    source ~/.bash_profile

  elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
    echo "Linux detected. Assuming that you are using bash as your default shell."
    echo "If this is not the case, then make sure to set the TOMCAT"\
    "environment variable to point to the tomcat/ directory."
    echo ""
    echo "# The following line has been automatically added to"\
    "your ~/.bashrc file:"
    echo ""
    echo "export TOMCAT=`pwd`"
    echo "# TOMCAT environment variable automatically set by TOMCAT installer." >> ~/.bashrc
    echo "export TOMCAT=`pwd`" >> ~/.bashrc
    source ~/.bashrc
  fi

else
  echo "TOMCAT environment variable is already set to ${TOMCAT}."
  echo "If this does not match `pwd`/tomcat, please correct it in"\
  "~/.bashrc (or ~/.bash_profile for MacOS."
fi

echo "Finished ToMCAT installation!"
