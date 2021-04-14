project(libmosquitto_example)
cmake_minimum_required(VERSION 3.10)

# The line below allows for finding the FindMosquitto.cmake find module in the
# tomcat/tools/cmake directory , so that we can then call find_package(Mosquitto
# REQUIRED) and set the correct compiler flags for the include directories and
# libraries to link to.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH}
                      "${CMAKE_SOURCE_DIR}/../../../../tools/cmake")

find_package(Mosquitto 1.5 REQUIRED)

add_executable(libmosquitto_example libmosquitto_example.c)
target_include_directories(libmosquitto_example
                           PRIVATE ${Mosquitto_INCLUDE_DIRS})
target_link_libraries(libmosquitto_example PRIVATE ${Mosquitto_LIBRARIES})
