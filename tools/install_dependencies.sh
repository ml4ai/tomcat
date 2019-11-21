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

