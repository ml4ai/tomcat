# tomcat-CDC

ToMCAT's Coordination Detection Component

To install the prerequisites using MacPorts:

```
sudo port selfupdate
sudo port install cmake boost paho.mqtt.cpp
```

To build:

```
mkdir build
cd build
cmake ..
make -j`
```

To run the program (assuming you are in the build directory)o

```
./main
```

To see available options:

```
./main -h
```
