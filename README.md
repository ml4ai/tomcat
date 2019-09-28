[![Build Status](https://travis-ci.com/ml4ai/tomcat.svg?token=sUdVniYKnyysze9kdsDd&branch=master)](https://travis-ci.com/ml4ai/tomcat)
[![codecov](https://codecov.io/gh/ml4ai/tomcat/branch/master/graph/badge.svg?token=OKXelvVO4z)](https://codecov.io/gh/ml4ai/tomcat)

# ToMCAT

This is the repository for the ToMCAT (Theory-of-Mind based Cognitive
Architecture for Teams) project at the University of Arizona.

### Dependencies

First, you'll need to grab the dependencies.

### MacOS

#### MacPorts

```
port install cmake libfmt doxygen boost ffmpeg opencv4 dlib openjdk8 gradle
```

Add the following line to your `~/.bash_profile` to make Java 8 the default
version:

```
export JAVA_HOME=/Library/Java/JavaVirtualMachines/openjdk8/Contents/Home
```

Then run `source ~/.bash_profile` to activate this Java version.


#### Homebrew

If you are using the Homebrew package manager, you can install these with the
following commands.

```
brew install cmake fmt doxygen boost ffmpeg opencv dlib
brew tap adoptopenjdk/openjdk
brew cask install adoptopenjdk8
brew install gradle
```


### WindowsOS

- Install Ubuntu alongside Windows (see https://tutorials.ubuntu.com/tutorial/tutorial-install-ubuntu-desktop#0 for a tutorial on how to install it).
- Boot up into the Ubuntu OS.
- Fire up a terminal (on Ubuntu, you can open a terminal with Ctrl+Alt+T).
- Install dependencies using apt-get - use the following commands

#### Ubuntu

```
sudo apt-get update
sudo apt-get install gcc-8 libfmt-dev doxygen ffmpeg libopencv-dev libdlib-dev openjdk-8-jdk
```

- ToMCAT requires Boost 1.69 or higher, so just install Boost from source (https://www.boost.org/doc/libs/1_71_0/more/getting_started/unix-variants.html)
- You'll need a newer version of Cmake than is available through apt. Use the following commands to do so.

```
curl -LO https://github.com/Kitware/CMake/releases/download/v3.15.3/cmake-3.15.3.tar.gz
tar -zxvf cmake-3.15.3.tar.gz
cd cmake-3.15.3
./bootstrap
make -j
sudo make -j install
```

- Navigate to the folder (or create the folder) where you want to install tomcat, using the cd command (if you are not familiar with basic shell commands, see here: https://swcarpentry.github.io/shell-novice/reference/ ).
- Follow the instructions here: https://github.com/ml4ai/tomcat#installation to install and run the ToMCAT agent.


Installation
------------

Clone the repo:

```
git clone https://github.com/ml4ai/tomcat
```


Malmo requires the environment variable `MALMO_XSD_PATH` to point to the Schemas directory.

For convenience, in your `~/.bash_profile` file, add:

```
export MALMO_XSD_PATH=<path_to_tomcat_repo>/external/malmo/Schemas
```

and make sure to run `source ~/.bash_profile` to activate the environment
variable.


To install the ToMCAT local agent, Malmo, and Minecraft, do the following:

```
cd tomcat
mkdir build && cd build 
cmake .. 
make -j
make -j Minecraft
```

Running experiments
-------------------

To launch Minecraft, execute the script (in the build directory)

```
./launch_minecraft.sh
```

Then in a separate terminal, run the executable `runExperiment`:

```
./bin/runExperiment --mission <path_to_mission_XML_file>
```

For example, to run the default world mission, you would do

```
./bin/runExperiment --mission ../external/malmo/sample_missions/default_world_1.xml
```

To allow human control of the character press the `<Enter>` key.

## For developers

To speed up builds, create a file called gradle.properties and add `org.gradle.daemon=true` to it:

```
echo "org.gradle.daemon=true" > ~/.gradle/gradle.properties.
```
