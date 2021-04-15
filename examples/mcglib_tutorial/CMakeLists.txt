cmake_minimum_required(VERSION 3.19)

project(mcglib_tutorial)

set(CMAKE_CXX_STANDARD 17)

find_package(nlohmann_json REQUIRED)

file(GLOB_RECURSE mcglib_tutorial_sources CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_executable(mcglib_tutorial ${mcglib_tutorial_sources})

target_include_directories(mcglib_tutorial PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../libs/mcglib/include)

if(NOT mcglib)
    add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../../libs/mcglib ${CMAKE_CURRENT_BINARY_DIR}/lib)
endif()

target_link_libraries(mcglib_tutorial mcglib nlohmann_json::nlohmann_json)
