file(GLOB_RECURSE project_sources CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/*.h ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

message(STATUS ${OpenCV_INCLUDE_DIRS})

add_library(project_static_lib STATIC ${project_sources})
target_include_directories(project_static_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${LSL_INCLUDE_DIR} ${Boost_INCLUDE_DIR} ${OpenCV_INCLUDE_DIRS})
set(_all_libs ${_libs} project_static_lib)

add_executable(video_capture main.cpp)
target_include_directories(video_capture PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(video_capture PRIVATE ${_all_libs})
