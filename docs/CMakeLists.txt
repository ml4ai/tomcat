find_package(Sphinx REQUIRED)

set(SPHINX_SOURCE ${CMAKE_CURRENT_SOURCE_DIR})
set(SPHINX_BUILD ${CMAKE_CURRENT_BINARY_DIR})
set(SPHINX_INDEX_FILE ${SPHINX_BUILD}/index.html)

add_custom_command(
  OUTPUT ${SPHINX_INDEX_FILE}
  COMMAND ${SPHINX_EXECUTABLE} -b html ${SPHINX_SOURCE} ${SPHINX_BUILD}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/index.rst ${DOXYGEN_INDEX_FILE}
  MAIN_DEPENDENCY ${SPHINX_SOURCE}/conf.py
  COMMENT "Generating documentation with Sphinx")

add_custom_target(sphinx ALL DEPENDS ${SPHINX_INDEX_FILE})
add_subdirectory(java_api)
add_custom_target(docs ALL DEPENDS sphinx java_apidocs)
