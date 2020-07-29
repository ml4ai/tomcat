file(GLOB tomcat_headers CONFIGURE_DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/*.h)

find_package(nlohmann_json REQUIRED)
add_library(tomcat STATIC ${tomcat_headers} LocalAgent.cpp Mission.cpp
                          FileHandler.cpp)

target_include_directories(tomcat PUBLIC ${Boost_INCLUDE_DIRS})

target_link_libraries(tomcat Malmo ${Boost_LIBRARIES} "z" fmt::fmt)

option(CODE_COVERAGE "Enable coverage reporting" OFF) 
# Todo make code coverage work on Linux as well.
if(APPLE
   AND CODE_COVERAGE
   AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  # Add required flags (GCC & LLVM/Clang)
  message("Building with code coverage")
  target_compile_options(
    tomcat -O0 # no optimization
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

# Add facesensor to build.
if(BUILD_FACESENSOR)
  add_subdirectory(faceSensor)
endif()


# Executable to extract the version number from the spec YAML file.
add_subdirectory(getSpecVersion)

# Procedural generation module.
add_subdirectory(pro_gen)
