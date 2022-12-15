# cpp_agent_template
#
# Every Dockerfile needs a FROM statement as the first executable line,
# this is the OS you will be using
FROM ubuntu:kinetic
CMD bash

# set this to avoid timezone prompt during apt-get installs
ENV DEBIAN_FRONTEND noninteractive

# Check https://packages.ubuntu.com/search?suite=all&arch=any&searchon=names&keywords=libvtk%20qt-dev
# for available packes for choosen UBUNTU_VERSION
ARG VTK_VERSION=6

# Install anything you need that is not included in your OS.
#RUN  apt-get update && apt-get -y install \
#    libssl-dev \
#    build-essential \
#    cmake \
#    cmake-gui \
#    cmake-curses-gui \
#    make \
#    doxygen \
#    git \
#    gcc \
#    wget \
#    fakeroot \
#    devscripts \
#    dh-make \
#    lsb-release 

RUN apt-get update \
 && apt-get -V install -y \
      build-essential \
      clang \
      clang-tidy \
      cmake \
      git \
      libbenchmark-dev \
      libblas-dev \
      libboost-date-time-dev \
      libboost-filesystem-dev \
      libboost-iostreams-dev \
      libflann-dev \
      libglew-dev \
      libgtest-dev \
      libopenni-dev \
      libopenni2-dev \
      libproj-dev \
      libqhull-dev \
      libqt5opengl5-dev \
      libusb-1.0-0-dev \
#      libvtk${VTK_VERSION}-dev \
#      libvtk${VTK_VERSION}-qt-dev \
      lsb-release \
      make \
      qtbase5-dev \
      software-properties-common \
      wget \
      xvfb 

# Install Paho c
RUN git clone --branch v1.3.8 https://github.com/eclipse/paho.mqtt.c.git

# BUILD PAHO MQTT C
# you will want to move this repo to the MacOS file path 
RUN cd paho.mqtt.c \
&& make install \
&& cmake -Bbuild -H. -DPAHO_ENABLE_TESTING=OFF -DPAHO_BUILD_STATIC=ON \
    -DPAHO_WITH_SSL=ON -DPAHO_HIGH_PERFORMANCE=ON \
&& cmake --build build/ --target install \
&& ldconfig \
&& cd ..

RUN export LD_LIBRARY_PATH=/paho.mqtt.c/src/:$LD_LIBRARY_PATH



# Install Paho cpp
RUN git clone https://github.com/eclipse/paho.mqtt.cpp.git

# BUILD PAHO MQTT CPP
# you will want to move this repo to the MacOS file path 
RUN cd paho.mqtt.cpp \
&& cmake -Bbuild -H. -DPAHO_BUILD_STATIC=ON \
&& cmake --build build/ --target install \
&& ldconfig \
&& cd ..

RUN export LD_LIBRARY_PATH=/paho.mqtt.cpp/build/src/:$LD_LIBRARY_PATH


# install Boost 1.74 that 1.79 will need
RUN cd /home && wget https://boostorg.jfrog.io/artifactory/main/release/1.74.0/source/boost_1_74_0.tar.gz \
&& tar xfz boost_1_74_0.tar.gz \
&& rm boost_1_74_0.tar.gz \
&& cd boost_1_74_0 \
&& ./bootstrap.sh --prefix=/usr/local --with-libraries=program_options \
&& ./b2 install \
&& ls -R  \
&& cd ..

RUN export LD_LIBRARY_PATH=/boost_1_74_0/:$LD_LIBRARY_PATH


# install Boost 1.79
RUN cd /home && wget https://boostorg.jfrog.io/artifactory/main/release/1.79.0/source/boost_1_79_0.tar.gz \
&& tar xfz boost_1_79_0.tar.gz \
&& rm boost_1_79_0.tar.gz \
&& cd boost_1_79_0 \
&& ./bootstrap.sh --prefix=/usr/local --with-libraries=program_options \
&& ./b2 install \
&& ls -R  \
&& cd ..

RUN export LD_LIBRARY_PATH=/boost_1_79_0/:$LD_LIBRARY_PATH


COPY . /cpp_agent_template
WORKDIR /cpp_agent_template

# Build the executable
#RUN g++ -Lpaho.mqtt.cpp/build/src *.cpp *.hpp -l paho-mqttpp3 -o cpp_agent_template_app
RUN rm -rf build \
&& mkdir build \
&& cd build \
&& cmake .. \
&& cd ..

ENTRYPOINT /cpp_agent_template/cpp_agent_template_app
