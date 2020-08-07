project(tomcat)

cmake_minimum_required(VERSION 3.10)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/tools/cmake/")

set(ENV{MALMO_XSD_PATH} external/malmo/Schemas)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/)

set(CMAKE_CXX_FLAGS
    "${CMAKE_CXX_FLAGS} -fvisibility=hidden -fvisibility-inlines-hidden")

cmake_policy(SET CMP0074 NEW)


add_subdirectory(external/malmo)

option(BUILD_FACESENSOR "Enable building of the facesensor executable" ON)
if(BUILD_FACESENSOR)
    add_subdirectory(external/OpenFace)
endif()

add_subdirectory(src/cpp)

target_include_directories(runMission PRIVATE external/malmo/Malmo/src)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tools/launch_minecraft
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

option(BUILD_DOCS "Build documentation" OFF)
if(BUILD_DOCS)
  add_subdirectory(docs)
endif()
