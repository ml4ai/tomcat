# Frame capture


## Prerequisites

Install portaudio. With MacPorts, you can use the invocation below.
```
sudo port install portaudio
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
cmake .. -DLSL_INCLUDE_DIR=/usr/local/include
make -j
```

The project will be built and the executable placed at `build/bin/`.

## Running the program.

### Command-line invocation:

```
./audio_capture --out_dir<directory to store the audio file>
```

- If the directory to store the audio file is not present, it will be created.
- Audio will be recorded by default with `1 channel (mono)`, at `48kHz` and `8192 chunk size`. It's possible to change these parameters with the extra program options. Do `./audio_capture -h` to see them.
- The number of bits per sample cannot be parametrized. It was fixed in 16 bits per sample for compatibility with other modules and the Google Speech-to-Text API.

# Stopping the program

To stop the program, use either a SIGINT or SIGTERM interruption. The program will perform proper clean-up and end gracefully. Other kinds of interruptions are not handled by the program and might cause loss of data.

## LSL

An LSL stream named Audio will be created when the program is executed. The same audio chunks saved to the audio file are also sent to LSL through that stream.
