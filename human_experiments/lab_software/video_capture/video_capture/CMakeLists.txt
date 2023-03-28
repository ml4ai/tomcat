# On macOS, there is a conflict between MacPorts OpenCV compiled using the
# Command Line Tools and XCode that might be installed on the user's computer.
# We attempt to ensure a consistent compilation experience by pointing CMake to
# the Command Line Tools instead of the XCode headers. This has to be set prior
# to the first project() command (see
# https://cmake.org/cmake/help/latest/variable/CMAKE_OSX_SYSROOT.html for more
# details).
#if(APPLE)
    #set(CMAKE_OSX_SYSROOT /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk)
#endif()

list(APPEND CMAKE_PREFIX_PATH "/opt/local/libexec/opencv4/cmake/")

find_package(OpenCV REQUIRED)
add_executable(video_capture main.cpp)
target_include_directories(video_capture PRIVATE ${OpenCV_INCLUDE_DIRS})
target_link_libraries(video_capture ${OpenCV_LIBRARIES})
