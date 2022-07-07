# Glob library (from https://github.com/p-ranav/glob)
set(CMAKE_CXX_STANDARD 23)
find_package(Glob REQUIRED)

add_definitions(-DBOOST_LOG_DYN_LINK)
find_package(Boost COMPONENTS log json REQUIRED)
add_executable(study3_eval study3_eval.cpp)
target_include_directories(
    study3_eval PRIVATE 
    Boost::json Boost::log
    ${Glob_INCLUDE_DIRS})
target_link_libraries(study3_eval Boost::log Boost::json Glob)
