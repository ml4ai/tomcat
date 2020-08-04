find_package(nlohmann_json REQUIRED)
find_package(Boost COMPONENTS program_options filesystem date_time REQUIRED QUIET)
add_executable(faceSensor WebcamSensor.cpp FaceSensor.cpp)
target_include_directories(faceSensor PRIVATE ${openface_include_dirs})
target_link_libraries(faceSensor PRIVATE OpenFace nlohmann_json::nlohmann_json
                                         ${Boost_LIBRARIES})
