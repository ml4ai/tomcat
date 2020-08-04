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
