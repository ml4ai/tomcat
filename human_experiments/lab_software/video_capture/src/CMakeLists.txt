file(GLOB_RECURSE project_sources CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/*.h ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_library(project_static_lib STATIC ${project_sources})
target_include_directories(project_static_lib PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${LSL_INCLUDE_DIR}
        ${Boost_INCLUDE_DIRS}
        ${OpenCV_INCLUDE_DIRS}
        ${portaudio_INCLUDE_DIR})
set(_all_libs ${_libs} project_static_lib)

add_executable(video_capture video_capture.cpp)
target_include_directories(video_capture PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${Boost_INCLUDE_DIRS})
target_link_libraries(video_capture PRIVATE ${_all_libs})

add_executable(audio_capture audio_capture.cpp)
target_include_directories(audio_capture PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${Boost_INCLUDE_DIRS})
target_link_libraries(audio_capture PRIVATE ${_all_libs})
