#!/bin/bash

set -e 

echo "Installing ToMCAT dependencies..."

# Check OS

# If MacOS, then try MacPorts and Homebrew

echo "Checking OS..."

if [[ "$OSTYPE" == "darwin"* ]]; then

  echo "MacOS detected. Checking for MacPorts or Homebrew package managers..."

  if [ -x "$(command -v port)" ]; then

    echo "'port' executable detected, assuming that MacPorts"\
    "(https://www.macports.org) is installed and is the package manager."

    echo "Installing ToMCAT dependencies using MacPorts..."

    sudo port selfupdate
    sudo port install \
      cmake \
      libfmt \
      doxygen \
      ffmpeg \
      opencv4 \
      dlib \
      openjdk8 \
      gradle \
      libsndfile \
      portaudio
    sudo port install boost -no_static

    # Check if JAVA_HOME is set to Java 8
    if ! [[ "$JAVA_HOME" == "/Library/Java/JavaVirtualMachines/openjdk8/Contents/Home" ]]; then
      echo "ToMCAT requires Java 8 to run. Add the following line to ~/.bash_profile"\
        "to make Java 8 the default version:"
      echo ""
      echo "    export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk8/Contents/Home"
      echo ""
      echo "Then run"
      echo ""
      echo "    source ~/.bash_profile"
      echo ""
      echo "to activate this Java version."
    fi

  elif [ -x "$(command -v brew)" ]; then

    echo "\'brew\' executable detected, assuming that Homebrew"\
    "\(https://brew.sh\) is installed and is the package manager."

    echo "Installing ToMCAT dependencies using Homebrew..."
    brew update
    brew install \
      cmake \
      fmt \
      doxygen \
      ffmpeg \
      opencv \
      dlib \
      boost \
      libsndfile \
      portaudio
      brew tap adoptopenjdk/openjdk
      brew cask install adoptopenjdk8
      brew install gradle
  fi
elif [ -x "$(command -v apt-get)" ]; then
  echo "apt-get executable found. Assuming that you are using a flavor of"\
  "Debian Linux, such as Ubuntu."
  echo ""
  echo "Installing dependencies using apt-get"
  sudo apt-get update
  sudo apt-get install \
    gcc-9 \
    libfmt-dev \
    doxygen \
    ffmpeg \
    libopencv-dev \
    libdlib-dev \
    openjdk-8-jdk \
    portaudio19-dev \
    libsndfile1-dev
fi

echo "Installing ToMCAT..."

./tools/download_tomcat_data.sh
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
  "~/.bash_profile."
fi

echo "Finished ToMCAT installation!"
