cmake_minimum_required(VERSION 3.16)
project(cpp_agent_template)
find_package(PahoMqttCpp REQUIRED)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
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
    cpp_agent_template main.cpp
    Agent.cpp
    MqttAgent.cpp
    FileAgent.cpp
    Processor.cpp
    ReferenceProcessor.cpp
    Configurator.cpp
    Utils.cpp
)

target_include_directories(cpp_agent_template PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(cpp_agent_template PahoMqttCpp::paho-mqttpp3 ${Boost_LIBRARIES})
