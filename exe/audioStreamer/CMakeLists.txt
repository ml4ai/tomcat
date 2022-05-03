project(audioStreamer)

cmake_minimum_required(VERSION 3.15)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
add_compile_definitions(BOOST_DATE_TIME_POSIX_TIME_STD_CONFIG)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
find_package(portaudio REQUIRED)
find_package(LibSndFile REQUIRED)
find_package(PahoMqttCpp REQUIRED)
find_package(nlohmann_json REQUIRED)

add_definitions(-DBOOST_LOG_DYN_LINK)
find_package(
    Boost 
    COMPONENTS 
    program_options
    log
    REQUIRED
)

add_executable(audioStreamer 
	src/main.cpp
	src/AudioStreamer.cpp	
	src/WebsocketClient.cpp
)

target_include_directories(audioStreamer PRIVATE 
    ${portaudio_INCLUDE_DIR}
    ${LIBSNDFILE_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS}
)

target_link_libraries(
    audioStreamer
    ${Boost_LIBRARIES}
    ${portaudio_LIBRARY}
    ${LIBSNDFILE_LIBRARIES}
    nlohmann_json::nlohmann_json
    PahoMqttCpp::paho-mqttpp3
    -pthread
)
