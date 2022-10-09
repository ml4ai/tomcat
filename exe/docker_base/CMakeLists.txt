#
# Paho MQTT C project
#

cmake_minimum_required(VERSION 3.16)
project(c_hello_world)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g") # create gdb symbol table


add_executable(
    c_hello_world main.c
)

