Multiplayer setup instructions
==============================

Instructions for running a ToMCAT mission with two or more players involved.

## Multiplayer configuration file

To build a connection between the mission server and clients, it is necessary
to specify their IP addresses and the port numbers in the
`conf/multiplayer_config.json` configuration file. Refer to the existing client
object structure inside and add the clients that will be connected, no more and
no less. That's because the ToMCAT mission will start only when all the clients
listed in this file are connected.

## Create mission instance on server side

The server agent does not actively participate in the mission, but it is needed
to create an instance of the mission on the server side. We will hide the
server agent later.  Use this command to launch the server-side mission
instance.

    ./external/malmo/Minecraft/launchClient.sh -port 10000

Here, `-port` specifies the port number for the server, which should be the
same as the one in the configuration file.

## Create mission instances on client side

To run the mission on a client machine, use this command:

    ./external/malmo/Minecraft/launchClient.sh -port x

Here, the `-port` option specifies the port numbers for the clients. Those
numbers need to be consistent with the ones in the configuration file as well.

## Starting the mission

After the server and all the client instances are ready, run this command on
the server side to start your mission:

    ./build/bin/runMission --mission 1 --multiplayer

The `--mission` flag specifies the mission to launch and the `--multiplayer`
flag will treat the mission as a multiplayer one.
