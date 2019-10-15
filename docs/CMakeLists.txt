# build the documentation from the C++ comments

find_package(Doxygen REQUIRED)
set(DOXYGEN_PROJECT_BRIEF "Theory of Mind-based Cognitive Architecture for Teams")
set(DOXYGEN_USE_MDFILE_AS_MAINPAGE "../README.md")
set(DOXYGEN_HTML_HEADER "header.html")
set(DOXYGEN_HTML_FOOTER "footer.html")
set(DOXYGEN_HTML_EXTRA_STYLESHEET "stylesheet.css")
set(DOXYGEN_HTML_EXTRA_FILES "doxy-boot.js")

doxygen_add_docs(
   docs ../README.md ../external/malmo/Minecraft/src
  COMMENT "Generating documentation with Doxygen..."
)
