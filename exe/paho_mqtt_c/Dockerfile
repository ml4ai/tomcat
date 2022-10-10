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

# you will want to move this repo to the MacOS file path 
RUN cd paho.mqtt.c \
&& rm -rf build  \
&& mkdir build  \
&& cd build \
&& cmake .. \
&& make -j `nproc` install \
&& cd ..





# build the paho c repo


RUN pwd
RUN ls

# Build the executable
RUN rm -rf build  \
&& mkdir build  \
&& cd build \
&& cmake -DPAHO_WITH_SSL=TRUE -DPAHO_BUILD_DOCUMENTATION=TRUE \
    -DPAHO_BUILD_SAMPLES=TRUE ../paho.mqtt.c 
#&& make -j `nproc` paho_mqtt_c  \
#&& cd .. 

# Run the executable
ENTRYPOINT ./build/paho_mqtt_c

