add_library(
  tomcat STATIC
  LocalAgent.cpp
  WebcamSensor.cpp
  MissionHandler.cpp
  MissionFactory.cpp
  Mission.cpp
  TomcatMission.cpp
  XMLMission.cpp
  FileHandler.cpp)

target_link_libraries(
  tomcat
  Malmo
  ${Boost_LIBRARIES}
  ${OpenCV_LIBS}
  OpenFace::GazeAnalyser
  OpenFace::LandmarkDetector
  OpenFace::Utilities
  "z"
  fmt::fmt)

if(CODE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  # Add required flags (GCC & LLVM/Clang)
  message("Building with code coverage")
  target_compile_options(
    tomcat
    PRIVATE -O0 # no optimization
            -g # generate debug info
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

set(BUILD_PYTHON_WRAPPER
    OFF
    CACHE BOOL "Build Python wrapper")

if(BUILD_PYTHON_WRAPPER)
  find_package(pybind11 REQUIRED)
  add_library(pytomcat MODULE Python_wrapper.cpp)
  target_link_libraries(pytomcat PRIVATE pybind11::module tomcat)
  target_include_directories(pytomcat PRIVATE ../external/malmo/Malmo/src)
  set_target_properties(
    pytomcat PROPERTIES PREFIX "${PYTHON_MODULE_PREFIX}" SUFFIX
                            "${PYTHON_MODULE_EXTENSION}")
endif()

add_executable(paex_record paex_record.c)
target_include_directories(paex_record PUBLIC "/opt/local/include")
target_link_directories(paex_record PUBLIC "/opt/local/lib")
target_link_libraries(paex_record portaudio)
