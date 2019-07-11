cmake_minimum_required(VERSION 3.11)
project(tomcat)

# Conan setup
include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

find_package(SWIG)
# Add and link pybind11 modules
add_subdirectory(external/malmo)
