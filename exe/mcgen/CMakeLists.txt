cmake_minimum_required(VERSION 3.19)

project(mcgen)

find_package(nlohmann_json REQUIRED)

file(GLOB_RECURSE mcgen_sources CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_executable(mcgen ${mcgen_sources})

target_include_directories(mcgen PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../../libs/mcglib/include)
target_link_libraries(mcgen mcglib nlohmann_json::nlohmann_json)
