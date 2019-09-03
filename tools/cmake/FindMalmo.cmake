# - Try to find Malmo
# Once done this will define
#  Malmo_FOUND - System has Malmo
#  Malmo_INCLUDE_DIRS - The Malmo include directories
#  Malmo_LIBRARY - The libraries needed to use Malmo

find_path(Malmo_INCLUDE_DIR
  NAMES "AgentHost.h"
  PATHS $ENV{Malmo_ROOT}
  PATH_SUFFIXES "include/malmo"
  NO_DEFAULT_PATH
)

find_library(Malmo_LIBRARY
  NAMES Malmo
  PATHS $ENV{Malmo_ROOT}
  PATH_SUFFIXES "lib"
  NO_DEFAULT_PATH
)

SET(Malmo_FOUND ON)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Malmo DEFAULT_MSG Malmo_LIBRARY Malmo_INCLUDE_DIR)

mark_as_advanced(Malmo_INCLUDE_DIR Malmo_LIBRARY)

message("
  Malmo information:
    Malmo headers found at ${Malmo_INCLUDE_DIR}
    Malmo library found at ${Malmo_LIBRARY}
")
