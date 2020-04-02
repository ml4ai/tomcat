Installation
============


The following commands should install ToMCAT and its dependencies for macOS and
Ubuntu (tested with Ubuntu 18.04) users:

```bash
git clone https://github.com/ml4ai/tomcat
cd tomcat && ./tools/install.sh
```

For people using other operating systems, note that ToMCAT
depends on the following: CMake 3.12+, Boost 1.65+, a C++17 compatible
compiler (tested with GCC 9 and AppleClang 11.0 so far), libfmt, doxygen,
ffmpeg, OpenCV, dlib, Java 8, and Gradle. You can inspect the script
`tools/install_dependencies.sh` to see what exactly is being installed and how.

Please make sure your internet connection is active while the `install.sh`
script is running. macOS users may want to turn off their firewalls.

We don't officially support Windows right now, but pull requests that add
Windows support are welcome.


Running experiments
-------------------

To run an experiment, run the following command from the `tomcat` directory.

    ./tools/run_session.sh

The data from the experiment will be saved in a folder whose name contains a
timestamp corresponding to the start of the experiment, of the form

    tomcat/data/participant_data/session_<Year>_<Month>_<Day>_<Hour>_<Minute>_<Second>

This folder will contain four files:
- `webcam_video.mpg` : A video recording of the player's face taken using the built-in
  webcam.
- `screen_video.mpg` : A video recording of the Minecraft playthrough (i.e.
  what the player sees on their screen)
- `player_audio.wav` : A WAV file containing the audio recording of the player.
- `malmo_data.tgz` : A gzipped tarball containing Malmo data about player's
  position, nearby entities, etc.
- `discrete_events.json`: A JSON file containing timestamped discrete events
  such as players interacting with doors, buttons, and levers, or attacking
  monsters.
- A JSON file with self-report survey responses.


To interrupt the game and quit at any time, press the `Esc` key, then click on
the terminal window where you ran the `run_session.sh` script, and then
interrupt the process with `Ctrl+C`. Then, run the following command to shut
down Minecraft.

```
./tools/kill_minecraft.sh
```

## For developers

To speed up builds, create a file called gradle.properties and add
`org.gradle.daemon=true` to it:

    echo "org.gradle.daemon=true" > ~/.gradle/gradle.properties
