# build the documentation from the C++ comments

find_package(Doxygen REQUIRED)
doxygen_add_docs(
  docs ../external/malmo/Minecraft/src
  COMMENT "Generating documentation with Doxygen..."
)
