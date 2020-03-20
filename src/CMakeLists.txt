add_library(
  tomcat STATIC
  utils.h
  utils.cpp
  LocalAgent.h
  LocalAgent.cpp
  Mission.h
  Mission.cpp
  FileHandler.h
  FileHandler.cpp)

target_link_libraries(
  tomcat
  Malmo
  ${Boost_LIBRARIES}
  "z"
  fmt::fmt
)

option(WITH_OPENFACE "Build with OpenFace integration" OFF)

if(WITH_OPENFACE)
find_package(OpenCV 4.0 REQUIRED COMPONENTS core imgproc calib3d
             highgui objdetect)

target_link_libraries(
  tomcat
  ${OpenCV_LIBS}
  OpenFace::GazeAnalyser
  OpenFace::LandmarkDetector
  OpenFace::Utilities
)
endif()

if(DEFINED ENV{TRAVIS})
  option(CODE_COVERAGE "Enable coverage reporting" ON)
  message("Compilation occurring on Travis, code coverage reporting on.")
endif()

if(CODE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  # Add required flags (GCC & LLVM/Clang)
  message("Building with code coverage")
  target_compile_options(
    tomcat
    PUBLIC -O0 # no optimization
            -g # generate debug info
            --coverage # sets all required flags
  )
  if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.13)
    target_link_options(tomcat PUBLIC --coverage)
  else()
    target_link_libraries(tomcat PUBLIC --coverage)
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
  set_target_properties(pytomcat PROPERTIES PREFIX "${PYTHON_MODULE_PREFIX}"
                                            SUFFIX "${PYTHON_MODULE_EXTENSION}")
endif()
