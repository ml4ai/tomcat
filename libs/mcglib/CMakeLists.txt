cmake_minimum_required(VERSION 3.10)

project(mcglib)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib/)

find_package(Boost COMPONENTS program_options REQUIRED)
find_package(nlohmann_json REQUIRED)

file(GLOB_RECURSE mcglib_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/include/*.h)
file(GLOB_RECURSE mcglib_core CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/src/core/*.cpp)

add_library(
  mcglib STATIC
  ${mcglib_core}
)

target_include_directories(mcglib PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/include")
target_link_libraries(mcglib ${Boost_LIBRARIES} nlohmann_json::nlohmann_json)
option(BUILD_EXAMPLES "Enable building of the example programs" OFF)
if(BUILD_EXAMPLES)
  add_subdirectory(examples)
endif()
