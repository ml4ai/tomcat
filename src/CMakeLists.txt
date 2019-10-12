add_library(tomcat STATIC LocalAgent.cpp WebcamSensor.cpp MissionHandler.cpp 
MissionFactory.cpp Mission.cpp TomcatMission.cpp XMLMission.cpp FileHandler.cpp)

target_link_libraries(
  tomcat
  Malmo
  ${Boost_LIBRARIES}
  ${OpenCV_LIBS}
  OpenFace::GazeAnalyser
  OpenFace::LandmarkDetector
  OpenFace::Utilities
  "z"
  fmt::fmt
)

if(CODE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  # Add required flags (GCC & LLVM/Clang)
  message("Building with code coverage")
  target_compile_options(tomcat PRIVATE
    -O0        # no optimization
    -g         # generate debug info
    --coverage # sets all required flags
  )
  if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.13)
    target_link_options(tomcat PRIVATE --coverage)
  else()
    target_link_libraries(tomcat PRIVATE --coverage)
  endif()
endif()

add_executable(runExperiment runExperiment.cpp)
target_link_libraries(runExperiment tomcat)

set(BUILD_PYTHON_WRAPPER OFF CACHE BOOL "Build Python wrapper")

if (BUILD_PYTHON_WRAPPER)
  find_package(pybind11 REQUIRED)
  add_library(tomcatPython MODULE Python_wrapper.cpp)
  target_link_libraries(tomcatPython PRIVATE pybind11::module tomcat)
  set_target_properties(tomcatPython PROPERTIES PREFIX "${PYTHON_MODULE_PREFIX}"
                                          SUFFIX "${PYTHON_MODULE_EXTENSION}")
endif()
