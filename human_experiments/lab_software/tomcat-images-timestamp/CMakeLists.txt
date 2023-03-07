cmake_minimum_required(VERSION 3.16)
set(CMAKE_CXX_STANDARD 20)

project(fileChecker)

find_package(Boost COMPONENTS program_options REQUIRED)

add_subdirectory(publish)

add_executable(${PROJECT_NAME} fileChecker.cpp)
target_include_directories(${PROJECT_NAME} PUBLIC publish)
target_link_libraries(${PROJECT_NAME} PRIVATE ${Boost_LIBRARIES} publish)
