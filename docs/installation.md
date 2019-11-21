Installation
============

ToMCAT depends on the following: CMake 3.15+, Boost 1.69+, a C++17 compatible
compiler (tested with GCC 9 and AppleClang 11.0 so far), libfmt, doxygen,
ffmpeg, OpenCV 4, dlib, Java 8, Gradle, libsndfile, and portaudio.

**Note:** On Debian-flavored Linux distros, you will need to install CMake and
Boost from source, since the versions installed by apt-get are not new enough
(as of 11/19/2019).

The following commands should work for users that use either MacPorts,
Homebrew, or apt-get as their package manager. 

```bash
git clone https://github.com/ml4ai/tomcat
cd tomcat && ./tools/install.sh
```

The scripts assume that the default shell is bash - if this is not the case for
you, then you are probably an advanced user, and may want to take a look at the
shell scripts before running them :).

We don't officially support Windows right now, but pull requests that add
Windows support are welcome.


Running experiments
-------------------

To launch Minecraft, execute the script (in the `build` directory)

    ./launch_minecraft.sh

Then in a separate terminal, run the executable `runExperiment`:

    ./bin/runExperiment --mission <path_to_mission_XML_file>

To run the default tutorial mission, just do:

    ./bin/runExperiment

To run the default search-and-rescue mission with a time limit of 10 minutes, you can just do:

    ./bin/runExperiment --mission 1 --time_limit 600

You can run `./bin/runExperiment --help` to see the other possible options.

If any of the following conditions happens, Minecraft needs to be relaunched
before another mission can be executed:

1. The player dies
2. The mission ends (either by timeout or by achievement of all the goals)

## For developers

To speed up builds, create a file called gradle.properties and add
`org.gradle.daemon=true` to it:

    echo "org.gradle.daemon=true" > ~/.gradle/gradle.properties

If you want to activate video recording and other things that depend on Malmo
quitting the server, uncomment line 18 in `src/TomcatMission.cpp` and
comment line 79 in 
`/external/malmo/Minecraft/src/main/java/edu/arizona/tomcat/Mission/Mission.java` file.

We are still working on this issue so we don't need to perform this step in the
future.
