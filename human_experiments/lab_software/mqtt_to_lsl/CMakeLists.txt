project(mqtt_to_lsl)
cmake_minimum_required(VERSION 3.10)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/tools/cmake/")

find_package(FMT REQUIRED)
find_package(nlohmann_json REQUIRED)

# Finding the Mosquitto header and library
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH}
        "${CMAKE_SOURCE_DIR}/../../../tools/cmake")
find_package(Mosquitto REQUIRED)

set(_libs
        nlohmann_json::nlohmann_json
        fmt::fmt
        ${Mosquitto_LIBRARIES})

add_subdirectory(src)
