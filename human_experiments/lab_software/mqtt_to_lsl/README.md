# MQTT to LSL


## Prerequisites

Install liblsl from source.
```
./tomcat/tools/install_from_source/liblsl
```

## Building the Project

If your current working directory is the same one this README is contained in,
you can use the commands below to build the project.

```
mkdir build
cd build
cmake .. -DLSL_INCLUDE_DIR=/usr/local/include
make -j
```

The project will be built and executables placed at `build/bin/`.

## Running the programs

### Minecraft MQTT to LSL:

This program connects to an MQTT server and subscribes to all topics. Upon 
message arrival, it concatenates the topic to the message (adds an extra field 
if the message is in json format) and pushes its string representation to the
an LSL stream called `Minecraft`.

```
./minecraft_mqtt_to_lsl --mqtt_addr "xxx.xxx.xxx.xxx" --mqtt_port yyyy
```

- Replace xxx.xxx.xxx.xxx with the IP address where the MQTT server is running
- Replace yyyy with the port where the MQTT server is running

### Publish Dummy Messages:

This program publishes 10,000 dummy json messages at 10Hz to the topic `dummy`
to the MQTT server it's connected to. It can be used to test if the MQTT to LSL 
programs are working properly.

```
./publish_dummy --mqtt_addr "xxx.xxx.xxx.xxx" --mqtt_port yyyy
```
