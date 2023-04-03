# Frame capture


## Prerequisites

Install OpenCV, ffmpeg, libfmt9, liblsl, and portaudio.

### MacPorts

With MacPorts, you can use the invocations below.

```
sudo port selfupdate
sudo port install opencv4 ffmpeg libfmt9 portaudio liblsl
```


### Installing LSL from source

If you don't have MacPorts, you can use the invocation below to install liblsl
from source.

```
./tomcat/tools/install_from_source/liblsl
```

## Building the project

If your current working directory is the same one this README is contained in,
you can use the commands below to build the project.

```
mkdir build
cd build
```

Then, do the following.

If you installed LSL using MacPorts:

```
cmake .. -Dfmt_DIR=/opt/local/lib/libfmt9/cmake/fmt -DOpenCV_DIR=/opt/local/libexec/opencv4/cmake
make -j
```

If you installed LSL from source:
```
cmake .. -Dfmt_DIR=/opt/local/lib/libfmt9/cmake/fmt -DOpenCV_DIR=/opt/local/libexec/opencv4/cmake -DLSL_INCLUDE_DIR=/usr/local/include
make -j
```

The project will be built and the executable placed at `build/bin/`.

## Running the program.

### Command-line invocation:

#### Recording from the webcam
```
./video_capture --device="webcam" --out_dir<directory to store frames> --fps=<frames per second> --camera_index=<index of the camera device>
```

or

```
./video_capture --device="webcam" --out_dir<directory to store frames> --fps=<frames per second> --camera_name=<name of the camera device>
```

The camera's index and name can be found with the command below:
```
ffmpeg -hide_banner -f avfoundation  -list_devices true -i dummy
```

#### Recording from the screen

```
./video_capture --device="screen" --out_dir<directory to store frames> --fps=<frames per second> --client="<unique name of the machine we are recording from>"`
```

- Frames per second should be an integer. This number is used to compute the number of milliseconds to wait between capturing two frames (`= 1000 / frames per second`).

#### Recording from the screen

```
./audio_capture --out_dir<directory to store the audio file>
```

- If the directory to store the audio file is not present, it will be created.
- Audio will be recorded by default with `1 channel (mono)`, at `48kHz` and `8192 chunk size`. It's possible to change this parameters with the extra program options. Do `./audio_capture -h` to see them.
- The number of bits per sample cannot be parametrized and it's fixed in 16 bits per sample for compatibility with other modules and Google Speech-to-Text API.


# Frame naming convention

`sequence#_yyyy-mm-dd_hh-mm-ss.milsec~gap.png`

- Time is in the *Universal Time Coordinated* (UTC)
- gap is the approximate number of milliseconds elapsed since the last frame till this frame was captured.

# Stopping the program

To stop the program, use either a SIGINT or SIGTERM interruption. The program will perform proper clean-up and end gracefully. Other kinds of interruptions are not handled by the program and might cause loss of data.

## LSL

Each device creates its own LSL stream and pushes data to it. The LSL stream names are defined below:

- Webcam
- Screen
- Audio
