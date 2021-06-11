cmake_minimum_required(VERSION 3.16)

project(mcgen)

find_package(nlohmann_json REQUIRED)

file(GLOB_RECURSE mcgen_sources CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_executable(mcgen ${mcgen_sources})

target_include_directories(mcgen PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../libs/mcg/include)
target_link_libraries(mcgen mcg nlohmann_json::nlohmann_json)
