# Frame capture


## Prerequisites

Install OpenCV and ffmpeg. With MacPorts, you can use the invocation below.
```
sudo port install opencv4 ffmpeg
```

Install liblsl from source.
```
./tomcat/tools/install_from_source/liblsl
```

## Building the project

If your current working directory is the same one this README is contained in,
you can use the commands below to build the project.

```
mkdir build
cd build
cmake .. -DOpenCV_DIR=/opt/local/libexec/opencv4/cmake/ -DLSL_INCLUDE_DIR=/usr/local/include
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

# Frame naming convention

`sequence#_yyyy-mm-dd_hh-mm-ss.milsec~gap.png`

- Sequence is a string of 6 digits containing the frame count prepended with leading zeros.
- Time is in the *Universal Time Coordinated* (UTC)
- gap is the approximate number of milliseconds elapsed since the last frame till this frame was captured.

# Stopping the program

To stop the program, use either a SIGINT or SIGTERM interruption. The program will perform proper clean-up and end gracefully. Other kinds of interruptions are not handled by the program and might cause loss of data.

## LSL

Each device creates its own LSL stream and pushes the image filenames to it. The LSL stream names are: 

- Webcam
- Screen
