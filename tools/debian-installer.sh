#!/bin/bash

DEPENDS=( nano curl git cmake libboostall-dev gcc libfmt-dev \
          doxygen ffmpeg libopencv-dev libdlib-dev openjdk-13-jdk \ 
          openjdk-13-jre gradle )
TOMCAT="$( cd "$( dirname "${BASH_SOURCE[0]}" )/../" >/dev/null 2>&1 && pwd )"

chkdepends() {
for dep in ${DEPENDS[@]}; do
   chkinstall ${dep}
done;
}

chkinstall() {
   package=`apt-cache policy $1 |grep -i installed`
   #installed=`grep -i installed $package`
   if [[ ${package} =~ "(none)" ]]; then 
      echo "$1: Installing ...";
      sudo apt-get -y install $1
   else
      echo "$1: ${package}";
   fi
}

debinstall() {
   if [ -f /etc/debian_version ]; then 
      . /etc/os-release
      echo "Installing ToMCAT dependencies.";
      if [[ $ID =~ "debian" ]]; then
         echo "debian found";
         chkdepends
      elif [[ $ID =~ "ubuntu" ]]; then
         echo "ubuntu found";
         sudo add-apt-repository -y ppa:ubuntu-toolchain-r/test
         sudo apt-get update
      if [[ $? -ne 0 ]]; then exit 1; fi;

      sudo apt-get install -y --allow-downgrades\
            cmake \
            gcc-9 \
            libfmt-dev \
            doxygen \
            ffmpeg \
            wmctrl \
            openjdk-8-jre-headless=8u162-b12-1\
            openjdk-8-jre=8u162-b12-1\
            openjdk-8-jdk-headless=8u162-b12-1\
            openjdk-8-jdk=8u162-b12-1
      if [[ $? -ne 0 ]]; then exit 1; fi;

      if [[ -z "$GITHUB_ACTIONS" ]]; then
         sudo apt-get install -y libboost-all-dev
      fi

      sudo update-java-alternatives -s java-1.8.0-openjdk-amd64
      echo "...";
      else 
         echo "Debian host not detected..";
         exit 1;
      fi
   fi
}


debinstall
[[ $? -ne 0 ]] || echo "ToMCAT dependency installation complete.\n ";

echo "Fetching ToMCAT"
git clone https://github.com/ml4ai/tomcat
cd tomcat && ./tools/install

