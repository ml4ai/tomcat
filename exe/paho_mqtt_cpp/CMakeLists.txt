#
# Paho MQTT C project
#

cmake_minimum_required(VERSION 3.16)
project(paho_mqtt_cpp_project)

set(PAHO_MQTT_C_LIBRARIES ~/paho_mqtt_cpp/paho.mqtt.c/build/output )
set(PAHO_MQTT_C_INCLUDE_DIRS ~/paho_mqtt_cpp/paho.mqtt.c/include )
set(PAHO_MQTT_CPP_LIBRARIES ~/paho_mqtt_cpp/paho.mqtt.cpp/build/output )
set(PAHO_MQTT_CPP_INCLUDE_DIRS ~/paho_mqtt_cpp/paho.mqtt.cpp/include )
set(CPP=/usr/bin/gcc )

find_package(
    OpenSSL
    REQUIRED
)

add_executable(
    paho_mqtt_cpp_app main.cpp
)
