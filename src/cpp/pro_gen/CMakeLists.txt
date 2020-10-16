cmake_minimum_required(VERSION 3.10)

project(progen)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
find_package(
  Boost 1.69
  COMPONENTS chrono
             date_time
             filesystem
             iostreams
             program_options
             regex
             system
             thread
  REQUIRED)

find_package(nlohmann_json REQUIRED)
file(GLOB pro_gen_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.h)
file(GLOB pro_gen_sources CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
add_library(
  progen STATIC
  ${pro_gen_headers}
  Pos.cpp
  Object.cpp
  AABB.cpp
  World.cpp
  Block.cpp
  Door.cpp
  Lever.cpp
  Entity.cpp
  Group.cpp
  Connection.cpp
  ZombieworldPit.cpp
  ZombieworldGroup.cpp
  ZombieWorld.cpp
  GridWorld.cpp)
target_link_libraries(progen ${Boost_LIBRARIES} nlohmann_json::nlohmann_json)
add_executable(generator generator.cpp)
target_link_libraries(generator progen)
