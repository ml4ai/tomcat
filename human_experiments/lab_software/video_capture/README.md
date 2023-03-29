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
cmake .. -DOpenCV_DIR=/opt/local/libexec/opencv4/cmake/
make -j
```

The program will be built and placed at `build/bin/video_capture`.

## Building using XCode

1. Create XCode command line tool C++ project for macOS
2. Add dynamic libraries
    1. Find the port install lacation of opencv4
       `$ port contents opencv4`
    2. Steps to add dynamic libraries
       1. Right click on the project folder
       2. Click - *Add files to ...*
       3. Press forward slash (/) on the keyboard
       4. In the dialog that opens, type the folder path
          E.g., `/opt/local/lib/opencv4/`
       5. From the file selection window that opens, select all the dynamic libraries (*.dylib)
3. Add header search path
   1. Select project folder
   2. Go to *Build settings*
   3. Select all and combined
   4. Search for *Header Search Paths*
   5. Double click on the right column (with heading as your project name) against the setting *Header Search Paths* and enter the path
      E.g., `/opt/local/include/opencv4/`
   6. Make it recursive
5. Enable camera access
   1. Expand *Products* folder on the *Project navigator* pane on the left margin of the XCode editor window.
   2. Right click on the file with your project name and click show in finder.
   3. Copy the *video_capture/info.plist* file into that folder.

Now the XCode project is ready to compile and run.

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