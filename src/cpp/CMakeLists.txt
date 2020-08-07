# Executable to run a ToMCAT mission.
add_subdirectory(runMission)

# Add facesensor to build.
if(BUILD_FACESENSOR)
  add_subdirectory(faceSensor)
endif()


# Executable to extract the version number from the spec YAML file.
add_subdirectory(getSpecVersion)

# Procedural generation module.
add_subdirectory(pro_gen)

# Optionally, build examples
option(BUILD_EXAMPLES "Enable building of the example programs" OFF)
if(BUILD_EXAMPLES)
    add_subdirectory(examples/libmosquitto_example)
endif()
