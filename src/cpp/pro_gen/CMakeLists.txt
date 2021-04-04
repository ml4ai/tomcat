cmake_minimum_required(VERSION 3.10)

project(progen)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Boost COMPONENTS program_options REQUIRED)
find_package(nlohmann_json REQUIRED)

file(GLOB_RECURSE pro_gen_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.h)
file(GLOB_RECURSE pro_gen_core CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/core/*.cpp)
file(GLOB_RECURSE pro_gen_zombieworld CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/ZombieWorld/*.cpp)
file(GLOB_RECURSE pro_gen_gridworld CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/GridWorld/*.cpp)
file(GLOB_RECURSE pro_gen_dungeonworld CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/DungeonWorld/*.cpp)
add_library(
  progen STATIC
  ${pro_gen_headers}
  ${pro_gen_core}
  ${pro_gen_zombieworld}
  ${pro_gen_gridworld}
  ${pro_gen_dungeonworld}
  )

target_link_libraries(progen ${Boost_LIBRARIES} nlohmann_json::nlohmann_json)
add_executable(generator generator.cpp)
target_link_libraries(generator progen)
