# build the documentation from the Java comments

find_package(Doxygen REQUIRED)
set(DOXYGEN_PROJECT_BRIEF "Theory of Mind-based Cognitive Architecture for Teams")
set(DOXYGEN_USE_MDFILE_AS_MAINPAGE "../README.md")
set(DOT_IMAGE_FORMAT "svg")
set(DOXYGEN_INTERACTIVE_SVG "YES")
set(DOXYGEN_HTML_OUTPUT ${CMAKE_CURRENT_BINARY_DIR})
#set(DOXYGEN_HTML_HEADER "header.html")
#set(DOXYGEN_HTML_FOOTER "footer.html")
#set(DOXYGEN_HTML_EXTRA_STYLESHEET "stylesheet.css")
#set(DOXYGEN_HTML_EXTRA_FILES "doxy-boot.js")

doxygen_add_docs(
  java_apidocs ../../README.md ../../external/malmo/Minecraft/src/main/java/edu
  COMMENT "Generating documentation with Doxygen..."
)
