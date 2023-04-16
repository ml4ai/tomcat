file(GLOB_RECURSE project_sources CONFIGURE_DEPENDS
        ${CMAKE_CURRENT_SOURCE_DIR}/*.h ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

add_library(project_static_lib STATIC ${project_sources})
target_include_directories(project_static_lib PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${LSL_INCLUDE_DIR}
        ${Mosquitto_INCLUDE_DIRS}
        ${Boost_INCLUDE_DIR})

target_link_libraries(project_static_lib
        PRIVATE
        ${Mosquitto_LIBRARIES}
        LSL::lsl
        ${Boost_LIBRARIES}
        nlohmann_json::nlohmann_json
        fmt::fmt
        -pthread)

add_executable(minecraft_mqtt_to_lsl minecraft_mqtt_to_lsl.cpp)
target_include_directories(minecraft_mqtt_to_lsl PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(minecraft_mqtt_to_lsl PRIVATE project_static_lib fmt::fmt)

add_executable(publish_dummy publish_dummy.cpp)
target_include_directories(publish_dummy PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(publish_dummy PRIVATE project_static_lib fmt::fmt)
