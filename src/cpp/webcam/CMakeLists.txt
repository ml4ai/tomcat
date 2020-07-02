find_package(nlohmann_json REQUIRED)
add_executable(ausensor WebcamSensor.cpp AUSensor.cpp)
target_include_directories(ausensor PRIVATE ${openface_include_dirs})
target_link_libraries(ausensor PRIVATE OpenFace nlohmann_json::nlohmann_json)
