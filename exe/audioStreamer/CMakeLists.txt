cmake_minimum_required(VERSION 3.19)

project(audioStreamer)

find_package(portaudio REQUIRED)
find_package(Boost REQUIRED)

add_executable(audioStreamer main.cpp)

target_include_directories(audioStreamer PRIVATE ${portaudio_INCLUDE_DIRS} ${Boost_INCLUDE_DIRS})

target_link_libraries(
    audioStreamer
    PRIVATE
    -pthread
    portaudio_static
)
