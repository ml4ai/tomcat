cmake_minimum_required(VERSION 3.11)

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

# Download and unzip the OpenFace models
set(MODELS ${CMAKE_CURRENT_BINARY_DIR}/models.zip)
download_and_unzip_tgz(http://vision.cs.arizona.edu/adarsh/OpenFace_models.tgz
                       ${MODELS} "OpenFace models")

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

find_package(OpenCV REQUIRED)

if(DEFINED ENV{TRAVIS})
  message("-- CMake running on Travis, using downloaded dlib.")
  add_subdirectory(external/dlib-19.17)
else()
  find_package(dlib REQUIRED)
endif()

add_subdirectory(external/malmo)
add_subdirectory(external/OpenFace)

if(DEFINED ENV{TRAVIS})
  option(CODE_COVERAGE "Enable coverage reporting" ON)
endif()

add_subdirectory(src)
target_include_directories(tomcat
                           PRIVATE src external/malmo/Malmo/src
                                   ${OpenFace_INCLUDE_DIRS})

target_include_directories(runExperiment PRIVATE external/malmo/Malmo/src)
file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/tools/launch_minecraft.sh
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory(tests)
target_include_directories(test PRIVATE src external/malmo/Malmo/src)
