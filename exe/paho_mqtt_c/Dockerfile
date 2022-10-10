# Every Dockerfile needs a FROM statement as the first executable line,
# this is the OS you will be using
FROM ubuntu:kinetic
CMD bash

# set this to avoid timezone prompt during apt-get installs
ENV DEBIAN_FRONTEND noninteractive

# Install anything you need that is not included in your OS.
RUN  apt-get update && apt-get -y install \
    libssl-dev \
    build-essential \
    cmake \
    cmake-gui \
    cmake-curses-gui \
    make \
    doxygen \
    git \
    gcc \
    fakeroot \
    devscripts \
    dh-make \
    lsb-release 

COPY . /paho_mqtt_c
WORKDIR /paho_mqtt_c

# Install Paho c
RUN git clone --branch v1.2.1 https://github.com/eclipse/paho.mqtt.c.git

# BUILD PAHO MQTT C
# you will want to move this repo to the MacOS file path 
RUN cd paho.mqtt.c \
&& make install \
&& cd build \
&& cmake -DPAHO_WITH_SSL=TRUE \
  -DPAHO_BUILD_DEB_PACKAGE=TRUE .. \
&& cmake --build . \
&& cd ../..

RUN pwd

RUN find -name *.cmake

RUN find -name *.so

RUN find -name *.h

RUN find -name *.a



# build the paho c repo


RUN pwd
RUN ls

RUN export LD_LIBRARY_PATH=/paho.mqtt.c/src/:$LD_LIBRARY_PATH

# Build the executable
RUN gcc -Lpaho.mqtt.c/build/output/ main.c -l paho-mqtt3c
#RUN rm -rf build  \
#&& mkdir build  \
#&& cd build \
#&& cmake .. \
#&& make -j `nproc` -I../paho.mqtt.c/src \
#&& cd .. 

# Run the executable
ENTRYPOINT ./build/paho_mqtt_c

