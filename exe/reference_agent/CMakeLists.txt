cmake_minimum_required(VERSION 3.16)
project(reference_agent)
#find_package(PahoMqttCpp REQUIRED)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g") # create gdb symbol table


# Add MacOs Boost directories
SET(CMAKE_INCLUDE_PATH ${CMAKE_INCLUDE_PATH} "/opt/local/libexec/boost/1.76/include/boost")
SET(CMAKE_LIBRARY_PATH ${CMAKE_LIBRARY_PATH} "/opt/local/libexec/boost/1.76/lib")

set(PAHO_MQTT_C_LIBRARIES ~/paho_mqtt_c/paho.mqtt.c/build/output )
set(PAHO_MQTT_C_INCLUDE_DIRS ~/paho_mqtt_c/paho.mqtt.c/include )
set(PAHO_MQTT_C_LIBRARIES ~/paho_mqtt_cpp/paho.mqtt.c/build/output )
set(PAHO_MQTT_C_INCLUDE_DIRS ~/paho_mqtt_cpp/paho.mqtt.c/include )
set(PAHO_MQTT_CPP_LIBRARIES ~/paho_mqtt_cpp/paho.mqtt.cpp/build/output )
set(PAHO_MQTT_CPP_INCLUDE_DIRS ~/paho_mqtt_cpp/paho.mqtt.cpp/include )
set(CPP=/usr/bin/gcc )


find_package(
    OpenSSL
    REQUIRED
)

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
    reference_agent 
    main.cpp 
    ReferenceProcessor.cpp
    FileAgent.cpp
    MqttAgent.cpp
    Agent.cpp
    Configurator.cpp
    Utils.cpp
    Processor.cpp
)


target_include_directories(reference_agent PRIVATE ${Boost_INCLUDE_DIRS} )
target_include_directories(reference_agent PRIVATE "/home/jastier/tomcat/exe/reference_agent" )
target_link_libraries(reference_agent PahoMqttCpp::paho-mqttpp3)
target_link_libraries(reference_agent ${Boost_LIBRARIES})
