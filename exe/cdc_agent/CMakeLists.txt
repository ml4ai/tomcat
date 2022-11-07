cmake_minimum_required(VERSION 3.16)
project(cdc_agent LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g") # create gdb symbol table

set(PAHO_MQTT_C_LIBRARIES ~/cdc_agent/paho.mqtt.c/build/output )
set(PAHO_MQTT_C_INCLUDE_DIRS ~/cdc_agent/paho.mqtt.c/include )
set(PAHO_MQTT_CPP_LIBRARIES ~/cdc_agent/paho.mqtt.cpp/build/output )
set(PAHO_MQTT_CPP_INCLUDE_DIRS ~/cdc_agent/paho.mqtt.cpp/include )

# Add MacOs Boost directories
SET(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH} "/opt/local/libexec/boost/1.76/include/boost")
SET(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} "/opt/local/libexec/boost/1.76/lib")


add_definitions(-DBOOST_LOG_DYN_LINK)

find_package(
    OpenSSL
    REQUIRED
)

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

add_executable(cdc_agent 
    main.cpp
    Agent.cpp
    MqttAgent.cpp
    FileAgent.cpp
    Processor.cpp
    CdcProcessor.cpp
    Configurator.cpp
    Utils.cpp
)

target_link_libraries(cdc_agent ${Boost_LIBRARIES})
target_include_directories(cdc_agent PRIVATE ${YAML_CPP_INCLUDE_DIR})
target_link_libraries(cdc_agent yaml-cpp)
