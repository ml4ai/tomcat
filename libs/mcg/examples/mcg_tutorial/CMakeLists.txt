cmake_minimum_required(VERSION 3.19)

project(mcg_tutorial)

set(CMAKE_CXX_STANDARD 17)

find_package(nlohmann_json REQUIRED)

add_executable(mcg_tutorial mcg_tutorial.cpp)

target_include_directories(mcg_tutorial PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../include)

target_link_libraries(mcg_tutorial mcg nlohmann_json::nlohmann_json)
