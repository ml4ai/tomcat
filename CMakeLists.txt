cmake_minimum_required(VERSION 3.11)
project(tomcat)

# Conan setup
include(${CMAKE_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

find_package(SWIG)
# Add and link pybind11 modules
add_subdirectory(external/malmo)
include_directories(external/malmo/Malmo/src)
add_executable(example example.cpp)
target_link_libraries(example ${CONAN_LIBS} Malmo)
#add_subdirectory(external/OpenFace)
