set(CMAKE_CXX_STANDARD 17)

find_package(Boost COMPONENTS program_options filesystem)
find_package(yaml-cpp REQUIRED)
add_executable(getSpecVersion getSpecVersion.cpp)
target_include_directories(getSpecVersion PRIVATE ${YAML_CPP_INCLUDE_DIR}
                                                  ${Boost_INCLUDE_DIRS})
target_link_libraries(getSpecVersion yaml-cpp ${Boost_LIBRARIES})
