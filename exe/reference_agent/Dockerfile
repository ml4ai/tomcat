
#FROM ubuntu:kinetic
# FROM ubuntu:jammy
FROM ubuntu:20.04

CMD bash

RUN apt-get update && apt-get install -y sudo software-properties-common g++ apt-utils
COPY . /tomcat/exe/reference_agent
#WORKDIR /tomcat/exe/reference_agent


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


# build paho mqtt c from source
RUN git clone https://github.com/eclipse/paho.mqtt.c.git \
    && cd paho.mqtt.c \
    && git checkout v1.3.8 \
    && cmake -Bbuild -H. -DPAHO_ENABLE_TESTING=OFF -DPAHO_BUILD_STATIC=ON \
    -DPAHO_WITH_SSL=ON -DPAHO_HIGH_PERFORMANCE=ON \
    && sudo cmake --build build/ --target install \
    && sudo ldconfig


# build paho mqtt cpp from source
RUN git clone https://github.com/eclipse/paho.mqtt.cpp.git \
    && cd paho.mqtt.cpp \
#    && git checkout v1.3.8 \
    && cmake -Bbuild -H. -DPAHO_BUILD_STATIC=ON -DPAHO_WITH_SSL=ON \
    && sudo cmake --build build/ --target install \
    && sudo ldconfig

# Build the agent
RUN pwd; rm -rf build; mkdir build; cd build; cmake ../ ; make -j `nproc` reference_agent; cd ../
