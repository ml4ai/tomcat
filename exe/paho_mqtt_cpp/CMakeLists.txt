#
# Paho MQTT CPP project
#

cmake_minimum_required(VERSION 3.16)
project(paho_mqtt_cpp_project)

find_package(
    OpenSSL
    REQUIRED
)


add_library(cpp_mqtt_lib SHARED IMPORTED)

# we use CMake in an ephemeral 'build' dir, so location is one level down
set_property(TARGET cpp_mqtt_lib PROPERTY IMPORTED_LOCATION "../paho.mqtt.cpp/build/src/libpaho-mqttpp3.so")

add_executable(
    paho_mqtt_cpp_app main.cpp
)

target_link_libraries(paho_mqtt_cpp_app cpp_mqtt_lib)
