# FindOpenFace.cmake
# Uses environment variable OpenFace_ROOT as backup
# - OpenFace_FOUND
# - OpenFace_include_DIRS
# - OpenFace_LIBRARIES

# message(${DEPendS_DIR})

find_path(OpenFace_include_DIRS
  LandmarkCoreIncludes.h
  GazeEstimation.h
  DOC "Found OpenFace include directory"
  PATHS
    "/usr/local/include/OpenFace"
    ENV OpenFace_ROOT
  # PATH_SUFFIXES
  #   include
)

find_library(LandmarkDetector
  NAMES libLandmarkDetector.a
  DOC "Found OpenFace library path"
  PATHS
    "/usr/local/lib"
    ENV OpenFace_ROOT
)

find_library(FaceAnalyser
  NAMES libFaceAnalyser.a
  DOC "Found OpenFace library path"
  PATHS
    "/usr/local/lib"
    ENV OpenFace_ROOT
  # PATH_SUFFIXES
  #   lib
  #   lib64
)

find_library(GazeAnalyser
  NAMES libGazeAnalyser.a
  DOC "Found OpenGaze library path"
  PATHS
    "/usr/local/lib"
    ENV OpenGaze_ROOT
  # PATH_SUFFIXES
  #   lib
  #   lib64
)

find_library(Utilities
  NAMES libUtilities.a
  DOC "Found OpenFace library path"
  PATHS
    "/usr/local/lib"
    ENV OpenFace_ROOT
  # PATH_SUFFIXES
  #   lib
  #   lib64
)

SET(OpenFace_LIBRARIES ${FaceAnalyser} ${GazeAnalyser} ${LandmarkDetector} ${Utilities})
message("OpenFace libs ${OpenFace_LIBRARIES}")


if(OpenFace_include_DIRS AND OpenFace_LIBRARIES)
  include(CheckCXXSourceCompiles)
  set(CMAKE_REQUIRED_includeS ${OpenFace_INCLUDE_DIRS})
  set(CMAKE_REQUIRED_LIBRARIES ${OpenFace_LIBRARIES})
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
  # check_cxx_source_compiles("#include <LandmarkCoreIncludes.h>\nint main(void) { return 0; }" OPENFACE_WORKS)
  set(OPENFACE_WORKS TRUE)
  set(CMAKE_REQUIRED_DEFINITIONS)
  set(CMAKE_REQUIRED_includeS)
  set(CMAKE_REQUIRED_LIBRARIES)
endif()

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(OpenFace FOUND_VAR OpenFace_FOUND
  REQUIRED_VARS OpenFace_LIBRARIES OpenFace_include_DIRS OPENFACE_WORKS)
