cmake_minimum_required(VERSION 3.10)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project(tomcat)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/tools/cmake/")

include(utils)

set(ENV{MALMO_XSD_PATH} external/malmo/Schemas)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/)

set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} -fvisibility=hidden -fvisibility-inlines-hidden")

cmake_policy(SET CMP0074 NEW)

find_package(FMT REQUIRED)

if(FMT_FOUND)
  message("-- fmt package found.")
else()
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

add_subdirectory(external/malmo)

add_subdirectory(src)
target_include_directories(tomcat PRIVATE src external/malmo/Malmo/src)
target_include_directories(runExperiment PRIVATE external/malmo/Malmo/src)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tools/launch_minecraft.sh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

option(BUILD_DOCS "Build documentation" OFF)
if (BUILD_DOCS)
  add_subdirectory(docs)
endif()
