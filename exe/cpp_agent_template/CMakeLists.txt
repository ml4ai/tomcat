cmake_minimum_required(VERSION 3.16)
project(ProjectName)
find_package(PahoMqttCpp REQUIRED)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g") # create gdb symbol table

add_definitions(-DBOOST_LOG_DYN_LINK)
find_package(
    Boost 
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
    BaseAgent.cpp 
    Configurator.cpp
    ReferenceAgent.cpp
)
target_link_libraries(main PahoMqttCpp::paho-mqttpp3 ${Boost_LIBRARIES})
