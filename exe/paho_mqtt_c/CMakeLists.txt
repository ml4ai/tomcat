#
# Paho MQTT C project
#

cmake_minimum_required(VERSION 3.16)
project(paho_mqtt_c LANGUAGES C)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} paho.mqtt.c/build/src )
set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} paho.mqtt.c/build/src )
#set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} eclipse-paho-mqtt-c )
set(eclipse-paho-mqtt-c_DIR paho.mqtt.c/build/ )


find_package(
    OpenSSL
    REQUIRED
)

#find_package(
#    eclipse-paho-mqtt-c
#    REQUIRED
#)


add_executable(
    paho_mqtt_c main.c
)

#target_link_libraries(paho_mqtt_c eclipse-paho-mqtt-c::paho-mqtt3as) 
