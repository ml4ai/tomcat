cmake_minimum_required(VERSION 3.11)
project(tomcat)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/tools/cmake/")

set(ENV{MALMO_XSD_PATH} external/malmo/Schemas)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/)

set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} -fvisibility=hidden -fvisibility-inlines-hidden")
cmake_policy(SET CMP0074 NEW)

include(utils)

# Download and unzip the OpenFace models
set(MODELS ${CMAKE_CURRENT_BINARY_DIR}/models.zip)
message("Downloading OpenFace models...")
download_file(http://vision.cs.arizona.edu/adarsh/models.zip ${MODELS})

execute_process(COMMAND ${CMAKE_COMMAND}
                        -E
                        tar
                        -xf
                        ${MODELS}
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

find_package(FMT REQUIRED)

if(NOT FMT_FOUND)
  message(
    "-- fmt package not found on system, please install it.  (https://fmt.dev)")
endif()

find_package(Boost 1.69
             COMPONENTS chrono
                        date_time
                        filesystem
                        iostreams
                        program_options
                        regex
                        system
                        thread
             REQUIRED)

find_package(OpenCV REQUIRED)

if(DEFINED ENV{TRAVIS})
  message("-- Installation running on Travis, using downloaded dlib.")
  add_subdirectory(external/dlib-19.17)
else()
  find_package(dlib REQUIRED)
endif()

add_subdirectory(external/malmo)
add_subdirectory(external/OpenFace)
include_directories(external/malmo/Malmo/src)
include_directories(${OpenFace_INCLUDE_DIRS})
add_subdirectory(src)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tools/launch_minecraft.sh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
