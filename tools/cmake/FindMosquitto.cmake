# - Try to find Mosquitto
# Once done this will define
#  Mosquitto_FOUND - System has Mosquitto
#  Mosquitto_INCLUDE_DIRS - The Mosquitto include directories
#  Mosquitto_LIBRARIES - The libraries needed to use Mosquitto
#  Mosquitto_DEFINITIONS - Compiler switches required for using Mosquitto

find_package(PkgConfig)
pkg_check_modules(PC_Mosquitto QUIET libmosquitto)
set(Mosquitto_DEFINITIONS ${PC_Mosquitto_CFLAGS_OTHER})

find_path(Mosquitto_INCLUDE_DIR mosquitto.h
          HINTS ${PC_Mosquitto_INCLUDEDIR} ${PC_Mosquitto_INCLUDE_DIRS}
          PATH_SUFFIXES mosquitto )

find_library(Mosquitto_LIBRARY NAMES libmosquitto mosquitto
             HINTS ${PC_Mosquitto_LIBDIR} ${PC_Mosquitto_LIBRARY_DIRS} )

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set Mosquitto_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(Mosquitto DEFAULT_MSG
                                  Mosquitto_LIBRARY Mosquitto_INCLUDE_DIR)

mark_as_advanced(Mosquitto_INCLUDE_DIR Mosquitto_LIBRARY )

set(Mosquitto_LIBRARIES ${Mosquitto_LIBRARY} )
set(Mosquitto_INCLUDE_DIRS ${Mosquitto_INCLUDE_DIR})
