cmake_minimum_required(VERSION 3.19)

project(audioInputCheck)

find_package(portaudio REQUIRED)

add_executable(audioInputCheck main.cpp)

target_include_directories(audioInputCheck PRIVATE ${portaudio_INCLUDE_DIRS})

target_link_libraries(
    audioInputCheck
    PRIVATE
    -pthread
    portaudio_static
)