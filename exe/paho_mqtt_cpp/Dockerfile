# paho_mqtt_cpp
#
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

RUN ls paho.mqtt.c/build/output/
RUN ls paho.mqtt.cpp/build


COPY . /paho_mqtt_cpp
WORKDIR /paho_mqtt_cpp

# Build the executable
RUN g++ -Lpaho.mqtt.cpp/build/src main.cpp -l paho-mqttpp3 -o paho_mqtt_cpp_app

ENTRYPOINT /paho_mqtt_cpp/paho_mqtt_cpp_app
