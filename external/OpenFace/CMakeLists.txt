cmake_minimum_required (VERSION 3.2)
project(OpenFace VERSION 2.0.2)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin/)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${PROJECT_SOURCE_DIR}/cmake/modules/")

# make sure we'll use OpenBLAS only: there's a header file naming difference
# between different implementations; so OpenFace wants OpenBLAS;
find_package(OpenBLAS REQUIRED)
if ( ${OpenBLAS_FOUND} )
    MESSAGE("OpenBLAS information:")
    MESSAGE("  OpenBLAS_LIBRARIES: ${OpenBLAS_LIB}")
else()
    MESSAGE(FATAL_ERROR "OpenBLAS not found in the system.")
endif()

if ( ${OpenBLAS_INCLUDE_FOUND} )
    MESSAGE("  OpenBLAS_INCLUDE: ${OpenBLAS_INCLUDE_DIR}")
else()
    MESSAGE(WARNING "OpenBLAS include not found in the system. Using the one vended with OpenFace.")
	set(OpenBLAS_INCLUDE_DIR "${CMAKE_SOURCE_DIR}/lib/3rdParty/OpenBLAS/include")
    MESSAGE("  OpenBLAS_INCLUDE: ${OpenBLAS_INCLUDE_DIR}")
endif()


if (${CMAKE_CXX_COMPILER_ID} STREQUAL "GNU")
    execute_process(COMMAND ${CMAKE_CXX_COMPILER} -dumpversion OUTPUT_VARIABLE GCC_VERSION)
    if (GCC_VERSION VERSION_LESS 4.7)
        set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x -msse -msse2 -msse3")
    else ()
        set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -msse -msse2 -msse3")
    endif ()
else ()
    set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -msse -msse2 -msse3")
endif ()



# suppress auto_ptr deprecation warnings
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
    add_compile_options("-Wno-deprecated-declarations")
endif()

# LandmarkDetector library
add_subdirectory(lib/local/LandmarkDetector)
# Facial Expression analysis library
add_subdirectory(lib/local/FaceAnalyser)
# Gaze estimation library
add_subdirectory(lib/local/GazeAnalyser)
# Utilities library
add_subdirectory(lib/local/Utilities)
