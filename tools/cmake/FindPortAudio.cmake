# - Try to find Portaudio
# Once done this will define
#
#  PORTAUDIO_FOUND - system has Portaudio
#  PORTAUDIO_INCLUDE_DIRS - the Portaudio include directory
#  PORTAUDIO_LIBRARIES - Link these to use Portaudio
#  PORTAUDIO_DEFINITIONS - Compiler switches required for using Portaudio
#  PORTAUDIO_VERSION - Portaudio version
#
#  Copyright (c) 2006 Andreas Schneider <mail@cynapses.org>
#
# Redistribution and use is allowed according to the terms of the New BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#


find_package(PkgConfig QUIET)
if(PKG_CONFIG_FOUND)
	pkg_check_modules(PORTAUDIO_PKGCONF portaudio-2.0)
endif(PKG_CONFIG_FOUND)

# Include dir
find_path(PORTAUDIO_INCLUDE_DIR
	NAMES portaudio.h
	PATHS ${PORTAUDIO_PKGCONF_INCLUDE_DIRS}
)

# Library
find_library(PORTAUDIO_LIBRARY
	NAMES portaudio
	PATHS ${PORTAUDIO_PKGCONF_LIBRARY_DIRS}
)

find_package(PackageHandleStandardArgs)
find_package_handle_standard_args(PortAudio DEFAULT_MSG  PORTAUDIO_LIBRARY PORTAUDIO_INCLUDE_DIR)

if(PORTAUDIO_FOUND)
  set(PORTAUDIO_LIBRARIES ${PORTAUDIO_LIBRARY})
  set(PORTAUDIO_INCLUDE_DIRS ${PORTAUDIO_INCLUDE_DIR})
endif(PORTAUDIO_FOUND)

mark_as_advanced(PORTAUDIO_LIBRARY PORTAUDIO_LIBRARIES PORTAUDIO_INCLUDE_DIR PORTAUDIO_INCLUDE_DIRS)
