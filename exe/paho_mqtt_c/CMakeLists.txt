#
# Paho MQTT C project
#

cmake_minimum_required(VERSION 3.5)
project(paho_mqtt_c)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} paho.mqtt.c/build/src/CMakeFiles/Export/lib/cmake/eclipse-paho-mqtt-c)
set(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} paho.mqtt.c/build/src)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} paho.mqtt.c/build/src/CMakeFiles/Export/lib/cmake/eclipse-paho-mqtt-c)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} paho.mqtt.c/build/src)
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g") # create gdb symbol table


find_package(
    eclipse-paho-mqtt-c
    REQUIRED
)

find_package(
    OpenSSL
    REQUIRED
)

add_executable(
    paho_mqtt_c main.c
)

#target_include_directories(paho_mqtt_hello_world PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(paho_mqtt_c eclipse-paho-mqtt-c::paho-mqtt3as)
