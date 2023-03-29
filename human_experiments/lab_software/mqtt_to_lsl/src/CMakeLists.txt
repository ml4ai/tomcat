file(GLOB_RECURSE project_sources CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/*.h ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_library(project_static_lib STATIC ${project_sources})
target_include_directories(project_static_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${LSL_DIR}/../../../include ${Mosquitto_INCLUDE_DIRS})
set(_all_libs ${_libs} project_static_lib)


add_executable(sync_usar sync_usar_data.cpp)
#target_sources(sync_usar PRIVATE ${project_sources})
target_include_directories(sync_usar PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(sync_usar PRIVATE ${_all_libs})

add_executable(send_random send_random.cpp)
#target_sources(send_random PRIVATE ${project_sources})
target_include_directories(send_random PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(send_random PRIVATE ${_all_libs})
