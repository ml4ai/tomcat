# Every Dockerfile needs a FROM statement as the first executable line,
# this is the OS you will be using
#FROM ubuntu:20.04
FROM ubuntu:kinetic
CMD bash

# set this to avoid timezone prompt during apt-get installs
ENV DEBIAN_FRONTEND noninteractive

RUN echo "Installing bash utils:"
# Install anything you need that is not included in your OS.
#RUN  apt-get update && apt-get -y install \
#    libssl-dev \
#    build-essential \
#    cmake \
#    cmake-gui \
#    cmake-curses-gui \
#    make \
#    git \
#    gcc \
#    fakeroot \
#    devscripts \
#    dh-make \
#    lsb-release 

RUN apt-get update && apt-get install -y sudo software-properties-common g++ apt-utils


RUN apt-get -y --no-install-recommends install \
      ca-certificates \
      build-essential \
      apt-utils \
      libboost-all-dev \
      pkg-config \
      cmake \
      curl \
      git \
      tar \
      wget \
      doxygen \
      graphviz \
      libgraphviz-dev \
      libsqlite3-dev \
      libeigen3-dev \
      libfmt-dev \
      libssl-dev \
      librange-v3-dev


RUN echo "Installation done:"
RUN pwd; ls


COPY . /cdc_agent
WORKDIR /cdc_agent

# Install Paho c
RUN echo "Cloning paho mqqt c"
RUN git clone --branch v1.2.1 https://github.com/eclipse/paho.mqtt.c.git

# Install Paho cpp
RUN echo "Cloning paho mqqt cpp"
RUN git clone https://github.com/eclipse/paho.mqtt.cpp.git

RUN echo "Cloning done:"
RUN pwd; ls

# build the paho c repo


#RUN echo "setting working directory:"
# set to project dir
#COPY . /paho_mqtt_c
#WORKDIR /paho_mqtt_c

RUN pwd; ls

# Build the executable
RUN echo "Building:"
RUN rm -rf build  \
&& mkdir build  \
&& cd build \
&& pwd; ls \
&& cmake .. \
&& make -j `nproc` paho_mqtt_c_test  \
&& cd .. 


RUN ls

