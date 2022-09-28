cmake_minimum_required(VERSION 3.16)
project(reference_agent)
find_package(PahoMqttCpp REQUIRED)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g") # create gdb symbol table

set(TEMPLATE_AGENT "/home/jastier/tomcat/exe/cpp_agent_template")


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
    reference_agent main.cpp 
    ReferenceProcessor.cpp
    ${TEMPLATE_AGENT}/FileAgent.hpp
    ${TEMPLATE_AGENT}/MqttAgent.hpp
    ${TEMPLATE_AGENT}/Agent.hpp
    ${TEMPLATE_AGENT}/Configurator.hpp
    ${TEMPLATE_AGENT}/Utils.hpp
    ${TEMPLATE_AGENT}/Processor.hpp
)


target_include_directories(reference_agent PRIVATE ${Boost_INCLUDE_DIRS} )
target_include_directories(reference_agent PRIVATE "/home/jastier/tomcat/exe/reference_agent" )
target_include_directories(reference_agent PRIVATE ${TEMPLATE_AGENT})
target_link_libraries(reference_agent PahoMqttCpp::paho-mqttpp3)
target_link_libraries(reference_agent ${Boost_LIBRARIES})
target_link_libraries(reference_agent libagent_template.a)
