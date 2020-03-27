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

target_include_directories(tomcat ${Boost_INCLUDE_DIRS})

target_link_libraries(
  tomcat
  Malmo
  ${Boost_LIBRARIES}
  "z"
  fmt::fmt
)


if(DEFINED ENV{TRAVIS})
  option(CODE_COVERAGE "Enable coverage reporting" ON)
  message("Compilation occurring on Travis, code coverage reporting on.")
endif()

# Todo make code coverage work on Linux as well.
if(APPLE AND CODE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  # Add required flags (GCC & LLVM/Clang)
  message("Building with code coverage")
  target_compile_options(
    tomcat
    -O0 # no optimization
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
