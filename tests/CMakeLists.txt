add_executable(test test.cpp)
target_link_libraries(test tomcat)

if(CODE_COVERAGE AND CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
  # Add required flags (GCC & LLVM/Clang)
  message("Building with code coverage")
  target_compile_options(test PRIVATE
    -O0        # no optimization
    -g         # generate debug info
    --coverage # sets all required flags
  )
  if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.13)
    target_link_options(test PRIVATE --coverage)
  else()
    target_link_libraries(test PRIVATE --coverage)
  endif()
endif()

