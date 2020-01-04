#!/bin/bash 

echo "Installing ToMCAT dependencies."

echo "Checking OS."
# If MacOS, then try MacPorts and Homebrew

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

        sudo port install \
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

        sudo port install boost -no_static
        if [[ $? -ne 0 ]]; then exit 1; fi;

    elif [ -x "$(command -v brew)" ]; then
        echo "\'brew\' executable detected, assuming that Homebrew"\
        "\(https://brew.sh\) is installed and is the package manager."
    
        echo "Installing ToMCAT dependencies using Homebrew."

        brew update
        if [[ $? -ne 0 ]]; then exit 1; fi;

        # We do not check exit codes for Homebrew installs since `brew install`
        # can return an exit code of 1 when a package is already installed (!!)

        brew install \
          cmake \
          fmt \
          doxygen \
          ffmpeg \
          opencv \
          openblas \
          boost \
          libsndfile \
          portaudio

        if [[ ! -z $TRAVIS ]]; then
          # The Homebrew install of DLib on Travis doesn't play well with CMake
          # for some reason, so we install DLib on Travis from source rather than
          # with the Homebrew package manager.
          pushd "${TOMCAT}/external"
            # We download a specific commit snapshot of dlib from Github that
            # contains a fix for the latest version of OpenCV that is being
            # installed by Homebrew.
            commit_sha=471c3d30e181a40942177a4358aa0496273d2108
            curl -L https://github.com/davisking/dlib/archive/${commit_sha}.zip -o dlib.zip
            unzip dlib.zip
            mv dlib-${commit_sha} dlib
          popd

          # On Travis, we will install lcov to provide code coverage estimates.
          brew install lcov
        else
          brew install dlib
        fi;

        # Installing Java
        brew tap adoptopenjdk/openjdk
        brew cask install adoptopenjdk8
        brew install gradle

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

    if [[ ! -z $TRAVIS ]]; then
      sudo apt purge --auto-remove cmake
      version=3.16
      build=2
      mkdir -p ~/temp
        pushd ~/temp
          wget https://cmake.org/files/v$version/cmake-$version.$build.tar.gz
          tar -xzvf cmake-$version.$build.tar.gz
          cd cmake-$version.$build/
          ./bootstrap
          make -j
          sudo make install
        popd
    fi

    sudo apt-get install \
        gcc-9 \
        libfmt-dev \
        doxygen \
        ffmpeg \
        libopenblas-dev \
        libopencv-dev \
        libdlib-dev \
        openjdk-8-jdk \
        portaudio19-dev \
        libsndfile1-dev
    if [[ $? -ne 0 ]]; then exit 1; fi;
else 
    echo "This is not a Mac and not Ubuntu (at least apt-get is not around). We cannot proceed."
    exit 1 
fi

echo "ToMCAT dependency installation complete."
echo " "
