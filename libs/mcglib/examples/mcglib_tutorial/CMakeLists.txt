cmake_minimum_required(VERSION 3.19)

project(mcglib_tutorial)

set(CMAKE_CXX_STANDARD 17)

find_package(nlohmann_json REQUIRED)

add_executable(mcglib_tutorial mcglib_tutorial.cpp)

target_include_directories(mcglib_tutorial PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../include)

target_link_libraries(mcglib_tutorial mcglib nlohmann_json::nlohmann_json)
