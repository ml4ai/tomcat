cmake_minimum_required(VERSION 3.16)
project(ProjectName)
find_package(PahoMqttCpp REQUIRED)
find_package(yaml-cpp REQUIRED)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g") # create gdb symbol table
add_definitions(-DBOOST_LOG_DYN_LINK)
find_package(
    Boost 1.79
    COMPONENTS 
    date_time
    chrono
    program_options
    coroutine
    json
    log
    REQUIRED
)

add_executable(
    main main.cpp
)
target_link_libraries(main PahoMqttCpp::paho-mqttpp3 ${Boost_LIBRARIES})
target_include_directories(main PRIVATE ${YAML_CPP_INCLUDE_DIR})
target_link_libraries(main yaml-cpp)

add_executable(test test.cpp)
target_include_directories(test PRIVATE ${Boost_INCLUDE_DIRS})
