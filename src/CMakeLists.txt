file(GLOB tomcat_headers ${CMAKE_CURRENT_SOURCE_DIR}/*.h)
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


if(WITH_WEBCAM_SENSOR)
  target_link_libraries(
    tomcat
    OpenFace
  )
  add_executable(webcam WebcamSensor.cpp WebcamSensor.h)
  target_include_directories(webcam PUBLIC )

  #target_include_directories(webcam PUBLIC
    #$<BUILD_INTERFACE:${CMAKE_PROJECT_DIR}/include>
    #$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include/OpenFace>)
  target_link_libraries(webcam fmt::fmt OpenFace)
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
