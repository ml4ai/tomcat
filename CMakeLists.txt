cmake_minimum_required(VERSION 3.11)
project(tomcat)

# Conan setup
include(${PROJECT_BINARY_DIR}/conanbuildinfo.cmake)
conan_basic_setup()

#==============================================================================
# Malmo integration
#==============================================================================

# Turning off the Java bindings for Malmo - Python bindings will still be
# built.
set(INCLUDE_JAVA OFF CACHE BOOL "Specifies whether to build Malmo with Java bindings")

# Adding the Malmo project as a subproject for ToMCAT
add_subdirectory(external/malmo)

# Adding the path to the Malmo header files
include_directories(external/malmo/Malmo/src)

# Compiling and linking an example C++ program with the Malmo libraries.
add_executable(example src/example.cpp)
target_link_libraries(example ${CONAN_LIBS} Malmo)

#==============================================================================
# OpenFace integration
#==============================================================================

# TODO The next two lines should theoretically add OpenFace to the project, but
# there are still some compilation issues to sort out.

# set(OpenCV_DIR external/OpenFace/lib/3rdParty/OpenCV3.4)
# add_subdirectory(external/OpenFace)
