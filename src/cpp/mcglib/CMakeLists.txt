cmake_minimum_required(VERSION 3.10)

project(mcglib)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Boost COMPONENTS program_options REQUIRED)
find_package(nlohmann_json REQUIRED)

file(GLOB_RECURSE mcglib_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.h)
file(GLOB_RECURSE mcglib_core CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/core/*.cpp)
file(GLOB_RECURSE mcglib_zombieworld CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ZombieWorld/*.cpp)
file(GLOB_RECURSE mcglib_gridworld CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/GridWorld/*.cpp)
file(GLOB_RECURSE mcglib_dungeonworld CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/DungeonWorld/*.cpp)
add_library(
  mcglib STATIC
  ${mcglib_headers}
  ${mcglib_core}
  ${mcglib_zombieworld}
  ${mcglib_gridworld}
  ${mcglib_dungeonworld}
  )

target_link_libraries(mcglib ${Boost_LIBRARIES} nlohmann_json::nlohmann_json)
add_executable(generator generator.cpp)
target_link_libraries(generator mcglib)
