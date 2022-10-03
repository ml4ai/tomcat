# Install Paho
WORKDIR /pahoc
RUN git clone --branch v1.2.1 https://github.com/eclipse/paho.mqtt.c.git
WORKDIR /pahoc/paho.mqtt.c
RUN make
RUN make install

# Install Paho C++
WORKDIR /pahocpp
RUN git clone --branch v1.0.0 https://github.com/eclipse/paho.mqtt.cpp
WORKDIR /pahocpp/paho.mqtt.cpp
RUN mkdir build
WORKDIR /pahocpp/paho.mqtt.cpp/build
RUN cmake -DPAHO_BUILD_DOCUMENTATION=FALSE -DPAHO_BUILD_SAMPLES=FALSE -DPAHO_MQTT_C_PATH=/pahoc/paho.mqtt.c ..
RUN make
RUN make install
