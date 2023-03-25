cmake_minimum_required(VERSION 3.16)
set(CMAKE_CXX_STANDARD 20)

project(mqtt_to_lsl)

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

add_executable(${PROJECT_NAME} sync_usar_data.cpp)
target_include_directories(${PROJECT_NAME} PUBLIC src)
target_link_libraries(${PROJECT_NAME} PRIVATE ${_libs})
