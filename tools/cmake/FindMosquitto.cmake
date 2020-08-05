# - Try to find Mosquitto
# Once done this will define
#  MOSQUITTO_FOUND - System has Mosquitto
#  MOSQUITTO_INCLUDE_DIRS - The Mosquitto include directories
#  MOSQUITTO_LIBRARIES - The libraries needed to use Mosquitto
#  MOSQUITTO_DEFINITIONS - Compiler switches required for using Mosquitto

find_package(PkgConfig)
pkg_check_modules(PC_MOSQUITTO QUIET libmosquitto)
set(MOSQUITTO_DEFINITIONS ${PC_MOSQUITTO_CFLAGS_OTHER})

find_path(MOSQUITTO_INCLUDE_DIR mosquitto.h
          HINTS ${PC_MOSQUITTO_INCLUDEDIR} ${PC_MOSQUITTO_INCLUDE_DIRS}
          PATH_SUFFIXES mosquitto )

find_library(MOSQUITTO_LIBRARY NAMES libmosquitto mosquitto
             HINTS ${PC_MOSQUITTO_LIBDIR} ${PC_MOSQUITTO_LIBRARY_DIRS} )

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set MOSQUITTO_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(Mosquitto  DEFAULT_MSG
                                  MOSQUITTO_LIBRARY MOSQUITTO_INCLUDE_DIR)

mark_as_advanced(MOSQUITTO_INCLUDE_DIR MOSQUITTO_LIBRARY )

set(MOSQUITTO_LIBRARIES ${MOSQUITTO_LIBRARY} )
set(MOSQUITTO_INCLUDE_DIRS ${MOSQUITTO_INCLUDE_DIR} )
