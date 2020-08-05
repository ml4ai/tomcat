project(libmosquitto_example)
cmake_minimum_required(VERSION 3.10)

# The line below allows for finding the FindMosquitto.cmake find module in the
# tomcat/tools/cmake directory , so that we can then call
# find_package(Mosquitto REQUIRED) and set the correct compiler flags for the
# include directories and libraries to link to.
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/../../../../tools/cmake")

find_package(Mosquitto REQUIRED)
message("Mosquitto header file found: ${Mosquitto_INCLUDE_DIRS}/mosquitto.h")
message("Mosquitto library found: ${Mosquitto_LIBRARIES}")
add_executable(example example.c)
target_include_directories(example PRIVATE ${Mosquitto_INCLUDE_DIRS})
target_link_libraries(example PRIVATE ${Mosquitto_LIBRARIES})
