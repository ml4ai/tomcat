# Frame capture


## Prerequisites

Install OpenCV. With MacPorts, you can use the invocation below.
```
sudo port install opencv4
```

You have two options for building the code: using XCode or CMake. Both are
described below.

## Building using CMake

If your current working directory is the same one this README is contained in,
you can use the commands below to build the project.

```
mkdir build
cd build
cmake .. -DOpenCV_DIR=/opt/local/libexec/opencv4/cmake/ -DOpenCV_DIR=/opt/local/libexec/opencv4/cmake/
make -j
```

## Running the program.

### Command-line invocation:

```
./video_capture <directory to store frames> <frames per second>`
```

- If the directory to store frames is not present, it will be created.
- Frames per minute should be an integer. This number is used to compute the number of milliseconds to wait between capturing two frames (`= 1000 / frames per second`).

### Testing using XCode

To test the program within XCode editor
* On the menu bar, click *Product > Scheme > Edit Scheme...*
* In the *Arguments Passed On Launch* dialog box, add the two arguments separated by a space


# Frame naming convention

`sequence#_yyyy-mm-dd_hh-mm-ss.milsec~gap.png`

- Time is in the *Universal Time Coordinated* (UTC)
- gap is the approximate number of milliseconds elapsed since the last frame till this frame was captured.

// https://stackoverflow.com/questions/12835577/how-to-convert-stdchronotime-point-to-calendar-datetime-string-with-fraction
// date.h: https://howardhinnant.github.io/date/date.html