#!/bin/bash

# Set the TOMCAT environment variable, assuming that the directory structure
# mirrors that of the git repository.
export TOMCAT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" >/dev/null 2>&1 && pwd )"

echo "Installing ToMCAT dependencies."

install_macports() {
  local version=2.6.2
  curl -O https://distfiles.macports.org/MacPorts/MacPorts-$version.tar.bz2
  tar xf MacPorts-$version.tar.bz2
  pushd MacPorts-$version > /dev/null
    ./configure
    make -j
    sudo make -j install
  popd > /dev/null
  rm -rf Macports-$version*
}

install_dependencies_using_macports() {
  echo "'port' executable detected, assuming that MacPorts"\
  "(https://www.macports.org) is installed and is the package manager."

  echo "Installing ToMCAT dependencies using MacPorts. If you are prompted for
  a password, please enter the password you use to install software on your
  macOS computer."

  sudo port selfupdate
  if [[ $? -ne 0 ]]; then exit 1; fi;

  sudo port -N install \
      cmake \
      libfmt \
      doxygen \
      ffmpeg \
      dlib \
      opencv4 \
      openblas \
      boost \
      gradle
  if [[ $? -ne 0 ]]; then exit 1; fi;

  # We install Java using a local Portfile, since the upstream openjdk8
  # port points to Java 1.8.0_242, which is incompatible with Malmo (the
  # local Portfile points to Java 1.8.0_232.
  pushd ${TOMCAT}/tools/local-ports/openjdk8 > /dev/null
    sudo port install
  popd > /dev/null
}

install_dependencies_using_homebrew() {
  echo "\'brew\' executable detected, assuming that Homebrew"\
  "\(https://brew.sh\) is installed and is the package manager."

  echo "Installing ToMCAT dependencies using Homebrew."

  brew update
  if [[ $? -ne 0 ]]; then exit 1; fi;

  # We do not check exit codes for Homebrew installs since `brew install`
  # can return an exit code of 1 when a package is already installed (!!)

  # We install Java using a local Homebrew formula, since the upstream openjdk8
  # formula points to Java 1.8.0_242, which is incompatible with Malmo (the
  # local formula points to Java 1.8.0_232).

  pushd ${TOMCAT}/tools/homebrew_formulae > /dev/null
    brew cask install adoptopenjdk8.rb
  popd > /dev/null

  brew install \
    cmake \
    fmt \
    doxygen \
    ffmpeg \
    opencv \
    openblas \
    boost \
    gradle

  if [[ ! -z $TRAVIS ]]; then
    # On Travis, we will install lcov to provide code coverage estimates.
    brew install lcov;
    download_and_extract_dlib
  else
    ./install_dlib_from_source.sh
  fi;
}

download_and_extract_dlib() {
  pushd "${TOMCAT}/external"
    # We download a specific commit snapshot of dlib from Github that
    # contains a fix for the latest version of OpenCV that is being
    # installed by Homebrew.
    commit_sha=471c3d30e181a40942177a4358aa0496273d2108
    curl -L https://github.com/davisking/dlib/archive/${commit_sha}.zip -o dlib.zip
    unzip dlib.zip
    mv dlib-${commit_sha} dlib
  popd
}

echo "Checking OS."
if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "macOS detected. Checking for macOS Command Line Tools."

    if [[ ! -d "/Library/Developer" ]]; then
      echo ""
      echo "[INFO]: The directory /Library/Developer was not found, so we"
      echo "assume that the macOS Command Line Tools are not installed."
      echo "Installing them now..."
      xcode-select --install
      osascript ${TOMCAT}/tools/install_macos_command_line_tools.scpt > /dev/null
      while [ ! -d "/Library/Developer" ]; do
        sleep 1
      done
      echo "macOS command line developer tools have been installed."
      echo ""
    fi

    echo "Checking for MacPorts or Homebrew package managers."
    macports_found=`[ -x "$(command -v port)" ]; echo $?`
    homebrew_found=`[ -x "$(command -v brew)" ]; echo $?` 

    if [[ $macports_found -eq 1 && $homebrew_found -eq 1 ]]; then
      echo "Neither the MacPorts or Homebrew package managers have been"
      echo "detected. Proceeding to install MacPorts in the default location"
      echo "(/opt/local)"
      install_macports
      install_dependencies_using_macports

    elif [[ $macports_found -eq 0 && $homebrew_found -eq 1 ]]; then
      install_dependencies_using_macports

    elif [[ $macports_found -eq 1 && $homebrew_found -eq 0 ]]; then
      install_dependencies_using_homebrew

    elif [[ $macports_found -eq 0 && $homebrew_found -eq 0 ]]; then
      echo "Both the MacPorts (https://www.macports.org) and Homebrew"
      echo "(https://brew.sh) package managers have been found. We assume you"
      echo "are a power user and can set your PATH environment variable as"
      echo "needed to switch between the two. We will proceed with installing"
      echo "the dependencies using MacPorts."
      install_dependencies_using_macports
    fi

elif [ -x "$(command -v apt-get)" ]; then
    echo "apt-get executable found. Assuming that you are using a flavor of"\
    "Debian Linux, such as Ubuntu."
    echo ""
    echo "Installing dependencies using apt-get"

    sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
    sudo apt-get update
    if [[ $? -ne 0 ]]; then exit 1; fi;

    # Install a version of CMake more up-to-date than what is available by
    # default for Ubuntu Bionic

    if [ -x "$(command -v cmake)" ]; then
      cmake_version=`cmake --version | head -n 1 | cut -d ' ' -f 3`
      cmake_major_version=`echo $cmake_version | cut -d '.' -f 1`
      cmake_minor_version=`echo $cmake_version | cut -d '.' -f 2`
      echo $cmake_version $cmake_major_version $cmake_minor_version
      if (( $cmake_major_version < 3 )) || (( $cmake_minor_version < 15 )); then
        ./tools/install_cmake_from_source.sh
      fi
    else
      ./tools/install_cmake_from_source.sh
    fi

    sudo apt-get install -y \
        gcc-9 \
        libfmt-dev \
        doxygen \
        ffmpeg \
        libopenblas-dev \
        openjdk-8-jre-headless=8u162-b12-1\
        openjdk-8-jre=8u162-b12-1\
        openjdk-8-jdk-headless=8u162-b12-1\
        openjdk-8-jdk=8u162-b12-1
    if [[ $? -ne 0 ]]; then exit 1; fi;

    boost_version_header="/usr/local/include/boost/version.hpp"
    if [[ -f $boost_version_header ]]; then
      boost_version=`cat $boost_version_header | grep "define BOOST_VERSION "\
                                               | cut -d' ' -f3`
      if (( $boost_version < 106900 )); then
        ./tools/install_boost_from_source.sh
      fi
    else
      ./tools/install_boost_from_source.sh
    fi

    if [[ ! -f "/usr/local/lib/libdlib.a" ]]; then
      ./tools/install_dlib_from_source.sh
    fi
    
    if [[ ! -d "/usr/local/include/opencv4" ]]; then
      ./tools/install_opencv_from_source.sh
    fi

else
    echo "This is not a macOS and not a Debian Linux distribution (at least"
    echo "apt-get is not around). We cannot proceed with the automated
    installation."
    exit 1
fi

echo "ToMCAT dependency installation complete."
echo " "
