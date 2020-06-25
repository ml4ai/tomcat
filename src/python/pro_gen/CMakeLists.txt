cmake_minimum_required(VERSION 3.10)
 
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
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
 
find_package(nlohmann_json REQUIRED)
file(GLOB pro_gen_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.h)
file(GLOB pro_gen_sources CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
add_library(progen STATIC ${pro_gen_headers} Pos.cpp AABB.cpp World.cpp Block.cpp ProceduralGenerator)
target_link_libraries(progen ${Boost_LIBRARIES} nlohmann_json::nlohmann_json)
add_executable(gridworld_generator gridworld_generator.cpp)
target_link_libraries(gridworld_generator progen)
