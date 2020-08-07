Installation
============


The following commands should install ToMCAT and its dependencies for macOS and
Ubuntu (tested with Ubuntu 18.04) users:

```bash
git clone https://github.com/ml4ai/tomcat
cd tomcat && ./tools/install
```

For people using other operating systems, note that ToMCAT depends on the
following: CMake 3.12+, Boost, a C++17 compatible compiler, libfmt, doxygen,
ffmpeg, OpenCV, dlib, Java 8, Mosquitto 1.5+, and Gradle. You can inspect the
script `tools/install_dependencies` to see what exactly is being installed and
how.

Please make sure your internet connection is active while the `install`
script is running. macOS users may want to turn off their firewalls.

We don't officially support Windows right now, but pull requests that add
Windows support are welcome.

## For developers

To speed up builds, create a file called gradle.properties and add
`org.gradle.daemon=true` to it:

    echo "org.gradle.daemon=true" > ~/.gradle/gradle.properties
