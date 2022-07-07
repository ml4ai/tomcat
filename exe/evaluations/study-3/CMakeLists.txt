find_package(Boost COMPONENTS json REQUIRED)
add_executable(study3_eval study3_eval.cpp)
target_include_directories(study3_eval PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(study3_eval ${Boost_LIBRARIES})
