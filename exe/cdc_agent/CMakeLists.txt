cmake_minimum_required(VERSION 3.16)
project(cdc_agent)
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
    cdc_agent main.cpp
    Agent.cpp
    MqttAgent.cpp
    FileAgent.cpp
    Processor.cpp
    CdcProcessor.cpp
    Configurator.cpp
    Utils.cpp

)
target_link_libraries(cdc_agent PahoMqttCpp::paho-mqttpp3 ${Boost_LIBRARIES})
target_include_directories(cdc_agent PRIVATE ${YAML_CPP_INCLUDE_DIR})
target_link_libraries(cdc_agent yaml-cpp)

add_executable(test test.cpp)
target_include_directories(test PRIVATE ${Boost_INCLUDE_DIRS})
