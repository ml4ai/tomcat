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
./video_capture --device="webcam" --out_dir<directory to store frames> --fps=<frames per second> --camera_index=<index of the camera device> --client="<unique name of the machine we are recording from>"`
```

or 

```
./video_capture --device="webcam" --out_dir<directory to store frames> --fps=<frames per second> --camera_name=<name of the camera device> --client="<unique name of the machine we are recording from>"`
```

The camera's index and name can be found with the command below:
```
ffmpeg -hide_banner -f avfoundation  -list_devices true -i dummy
```

#### Recording from the screen

```
./video_capture --device="screen" --out_dir<directory to store frames> --fps=<frames per second> --client="<unique name of the machine we are recording from>"`
```

- If the directory to store frames is not present, it will be created.
- Frames per second should be an integer. This number is used to compute the number of milliseconds to wait between capturing two frames (`= 1000 / frames per second`).
- The client must be a unique string that identifies the machine. The client name will be used to uniquely identify the LSL streams from the clients. The LSL streams will have the names Webcam_<client_name> and Screen_<client_name>. 

IMPORTANT:
- The client_name cannot have white spaces as the current implementation of the LSL library does not support stream names with white spaces.


# Frame naming convention

`sequence#_yyyy-mm-dd_hh-mm-ss.milsec~gap.png`

- Time is in the *Universal Time Coordinated* (UTC)
- gap is the approximate number of milliseconds elapsed since the last frame till this frame was captured.
