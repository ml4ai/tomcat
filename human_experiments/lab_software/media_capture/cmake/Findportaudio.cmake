# Distributed under the OSI-approved BSD 3-Clause License.  See accompanying
# file Copyright.txt or https://cmake.org/licensing for details.

#[=======================================================================[.rst:
Findportaudio
-------

Finds the portaudio library.

Imported Targets
^^^^^^^^^^^^^^^^

This module provides the following imported targets, if found:

``portaudio::portaudio``
  The portaudio library

Result Variables
^^^^^^^^^^^^^^^^

This will define the following variables:

``portaudio_FOUND``
  True if the system has the portaudio library.
``portaudio_VERSION``
  The version of the portaudio library which was found.
``portaudio_INCLUDE_DIRS``
  Include directories needed to use portaudio.
``portaudio_LIBRARIES``
  Libraries needed to link to portaudio.

Cache Variables
^^^^^^^^^^^^^^^

The following cache variables may also be set:

``portaudio_INCLUDE_DIR``
  The directory containing ``portaudio.h``.
``portaudio_LIBRARY``
  The path to the portaudio library.

#]=======================================================================]
find_package(PkgConfig)
pkg_check_modules(PC_portaudio QUIET portaudio)
find_path(portaudio_INCLUDE_DIR
  NAMES portaudio.h
  PATHS ${PC_portaudio_INCLUDE_DIRS}
  PATH_SUFFIXES portaudio
)
find_library(portaudio_LIBRARY
  NAMES portaudio
  PATHS ${PC_portaudio_LIBRARY_DIRS}
)
find_library(portaudio_LIBRARY_RELEASE
  NAMES portaudio
  PATHS ${PC_portaudio_LIBRARY_DIRS}/Release
)
find_library(portaudio_LIBRARY_DEBUG
  NAMES portaudio
  PATHS ${PC_portaudio_LIBRARY_DIRS}/Debug
)

include(SelectLibraryConfigurations)
select_library_configurations(portaudio)
set(portaudio_VERSION ${PC_portaudio_VERSION})
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(portaudio
  FOUND_VAR portaudio_FOUND
  REQUIRED_VARS
    portaudio_LIBRARY
    portaudio_INCLUDE_DIR
  VERSION_VAR portaudio_VERSION
)
if(portaudio_FOUND)
  set(portaudio_LIBRARIES ${portaudio_LIBRARY})
  set(portaudio_INCLUDE_DIRS ${portaudio_INCLUDE_DIR})
  set(portaudio_DEFINITIONS ${PC_portaudio_CFLAGS_OTHER})
endif()
if(portaudio_FOUND AND NOT TARGET portaudio::portaudio)
  add_library(portaudio::portaudio UNKNOWN IMPORTED)
  set_target_properties(portaudio::portaudio PROPERTIES
    IMPORTED_LOCATION "${portaudio_LIBRARY}"
    INTERFACE_COMPILE_OPTIONS "${PC_portaudio_CFLAGS_OTHER}"
    INTERFACE_INCLUDE_DIRECTORIES "${portaudio_INCLUDE_DIR}"
  )
endif()
mark_as_advanced(
  portaudio_INCLUDE_DIR
  portaudio_LIBRARY
)
