find_package(nlohmann_json REQUIRED)
add_executable(facesensor WebcamSensor.cpp FaceSensor.cpp)
target_include_directories(facesensor PRIVATE ${openface_include_dirs})
target_link_libraries(facesensor PRIVATE OpenFace nlohmann_json::nlohmann_json)
