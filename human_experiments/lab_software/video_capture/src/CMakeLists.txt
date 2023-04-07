file(GLOB_RECURSE project_sources CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/*.h ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_library(project_static_lib STATIC ${project_sources})
target_include_directories(project_static_lib PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${LSL_INCLUDE_DIR}
        ${Boost_INCLUDE_DIRS}
        ${OpenCV_INCLUDE_DIRS})
target_link_libraries(project_static_lib PRIVATE fmt::fmt)

add_executable(video_capture video_capture.cpp)
target_include_directories(
        video_capture
        PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${Boost_INCLUDE_DIRS}
        ${OpenCV_INCLUDE_DIRS}
)
target_link_libraries(
        video_capture
        PRIVATE
        ${Boost_LIBRARIES}
        ${OpenCV_LIBRARIES}
        project_static_lib
        fmt::fmt
        LSL::lsl
)
