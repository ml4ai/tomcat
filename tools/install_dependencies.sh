#!/bin/bash

echo "Installing ToMCAT dependencies."

echo "Checking OS."
# If MacOS, then try MacPorts and Homebrew

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

if [[ "$OSTYPE" == "darwin"* ]]; then
    echo "MacOS detected. Checking for XCode developer kit."

    XCode_sdk_dir="/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs"
    if [[ ! -d "${XCode_sdk_dir}" ]]; then
        echo "No XCode developer kit found. You need to get this from the app store."
        exit 1
    else
        if [[ ! -e "${XCode_sdk_dir}/MacOSX10.14.sdk" ]]; then
            if [[ ! -e "${XCode_sdk_dir}/MacOSX.sdk" ]]; then
                echo "No MacOSX.sdk in ${XCode_sdk_dir}."
                echo "Possibly MacOS has changed things (again)."
                exit 1
            else
                pushd "${XCode_sdk_dir}" > /dev/null
                    echo "Linking missing MacOSX10.14.sdk to MacOSX.sdk in ${XCode_sdk_dir}/"
                    sudo ln -s "MacOSX.sdk" "MacOSX10.14.sdk"
                    if [[ $? -ne 0 ]]; then exit 1; fi;
                popd > /dev/null
            fi
        fi
    fi

    echo "Found XCode developer kit."
    echo "Checking for MacPorts or Homebrew package managers."

    if [ -x "$(command -v port)" ]; then
        echo "'port' executable detected, assuming that MacPorts"\
        "(https://www.macports.org) is installed and is the package manager."

        echo "Installing ToMCAT dependencies using MacPorts."

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
            openjdk8 \
            gradle \
            libsndfile \
            portaudio
        if [[ $? -ne 0 ]]; then exit 1; fi;

        sudo port -N install boost -no_static
        if [[ $? -ne 0 ]]; then exit 1; fi;

    elif [ -x "$(command -v brew)" ]; then
        echo "\'brew\' executable detected, assuming that Homebrew"\
        "\(https://brew.sh\) is installed and is the package manager."

        echo "Installing ToMCAT dependencies using Homebrew."

        brew update
        if [[ $? -ne 0 ]]; then exit 1; fi;

        # We do not check exit codes for Homebrew installs since `brew install`
        # can return an exit code of 1 when a package is already installed (!!)

        brew tap AdoptOpenJDK/openjdk
        brew cask install adoptopenjdk8

        brew install \
          cmake \
          fmt \
          doxygen \
          ffmpeg \
          opencv \
          openblas \
          boost \
          libsndfile \
          portaudio \
          gradle

        if [[ ! -z $TRAVIS ]]; then
          # On Travis, we will install lcov to provide code coverage estimates.
          brew install lcov;
          download_and_extract_dlib
        else
          ./install_dlib_from_source.sh
        fi;
    else
        echo "No package manager found for $OSTYPE"
        exit 1
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
      cmake_major_version=`$cmake_version | cut -d '.' -f 1`
      cmake_minor_version=`$cmake_version | cut -d '.' -f 2`
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
        openjdk-8-jdk \
        portaudio19-dev \
        libsndfile1-dev
    if [[ $? -ne 0 ]]; then exit 1; fi;

    boost_version_header="/usr/local/include/boost/version.hpp"
    if [[ -f $boost_version_header ]]; then
      boost_version=`cat $boost_version_header | grep "define BOOST_VERSION " | cut -d' ' -f3`
      if (( $boost_version < 106900 )); then
        ./tools/install_boost_from_source.sh
      fi
    else
      ./tools/install_boost_from_source.sh
    fi

    if [[ ! -f "usr/local/lib/libdlib.a" ]]; then
      ./tools/install_dlib_from_source.sh
    fi
    
    if [[ ! -d "/usr/local/include/opencv4" ]]; then
      ./tools/install_opencv_from_source.sh
    fi

else
    echo "This is not a Mac and not Ubuntu (at least apt-get is not around). We cannot proceed."
    exit 1
fi

echo "ToMCAT dependency installation complete."
echo " "
