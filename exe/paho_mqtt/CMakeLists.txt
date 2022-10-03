cmake_minimum_required(VERSION 3.16)
project(paho_mqtt_hello_world)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g") # create gdb symbol table


find_package(
    eclipse-paho-mqtt-c
    REQUIRED
)

find_package(
    eclipse-paho-mqtt-cpp
    REQUIRED
)

add_executable(
    paho_mqtt_hello_world main.cpp
)

#target_include_directories(paho_mqtt_hello_world PRIVATE ${Boost_INCLUDE_DIRS})
#target_link_libraries(paho_mqtt_hello_world eclipse-paho-mqtt-c::paho-mqtt3as ${Boost_LIBRARIES})
