add_library(LocalAgent STATIC LocalAgent.cpp WebcamSensor.cpp)

target_link_libraries(
  LocalAgent 
  Malmo
  ${Boost_LIBRARIES}
  ${OpenCV_LIBS}
  OpenFace::GazeAnalyser
  OpenFace::LandmarkDetector
  OpenFace::Utilities
  "z"
  fmt::fmt
)

add_executable(runExperiment runExperiment.cpp)
target_link_libraries(runExperiment LocalAgent)

set(BUILD_PYTHON_WRAPPER OFF CACHE BOOL "Build Python wrapper")

if (BUILD_PYTHON_WRAPPER)
  find_package(pybind11 REQUIRED)  # or add_subdirectory(pybind11)
  add_library(tomcatPython MODULE Python_wrapper.cpp)
target_link_libraries(tomcatPython PRIVATE pybind11::module LocalAgent)
set_target_properties(tomcatPython PROPERTIES PREFIX "${PYTHON_MODULE_PREFIX}"
                                         SUFFIX "${PYTHON_MODULE_EXTENSION}")
endif()
