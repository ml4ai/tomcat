project(runMission)
cmake_minimum_required(VERSION 3.12)

file(GLOB tomcat_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.h)
file(GLOB tomcat_sources CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)

find_package(nlohmann_json REQUIRED)
find_package(FMT 6 REQUIRED)

add_executable(
  runMission ${tomcat_sources}
)

target_include_directories(runMission PUBLIC ${Boost_INCLUDE_DIRS})

target_link_libraries(
  runMission
  Malmo
  ${Boost_LIBRARIES}
  "z"
  fmt::fmt
)

option(CODE_COVERAGE "Enable coverage reporting" OFF)

# Todo make code coverage work on Linux as well.
if(APPLE AND CODE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  # Add required flags (GCC & LLVM/Clang)
  message("Building with code coverage")
  target_compile_options(
    runMission
    -O0 # no optimization
    -g # generate debug info
    --coverage # sets all required flags
  )
  if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.13)
    target_link_options(runMission PUBLIC --coverage)
  else()
    target_link_libraries(runMission PUBLIC --coverage)
  endif()
endif()
