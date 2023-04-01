file(GLOB_RECURSE project_sources CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/*.h ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_library(project_static_lib STATIC ${project_sources})
target_include_directories(project_static_lib PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${LSL_INCLUDE_DIR} ${Mosquitto_INCLUDE_DIRS} ${Boost_INCLUDE_DIR})
set(_all_libs ${_libs} project_static_lib)

add_executable(minecraft_mqtt_to_lsl minecraft_mqtt_to_lsl.cpp)
target_include_directories(minecraft_mqtt_to_lsl PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(minecraft_mqtt_to_lsl PRIVATE ${_all_libs})

add_executable(publish_dummy publish_dummy.cpp)
target_include_directories(publish_dummy PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(publish_dummy PRIVATE ${_all_libs})
