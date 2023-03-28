file(GLOB_RECURSE project_sources CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/*.h ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
add_library(project_lib STATIC ${project_sources})
target_include_directories(project_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${Mosquitto_INCLUDE_DIRS})

set(_all_libs project_lib ${_libs})

add_executable(sync_usar sync_usar_data.cpp)
target_include_directories(sync_usar PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(sync_usar PRIVATE ${_all_libs})
