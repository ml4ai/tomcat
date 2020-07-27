# Multiplayer setup instructions

Instructions for running TOMCAT mission with two or more players involved.

## Multiplayer configuration file

To build a connection between the mission server and clients, it is necessary
to specify the IP addresses and the port numbers of the server and the clients.
Thus, at the very beginning, go to the configuration file
`docs/multiplayer_config.json` file specify them. Refer to the existing client
object structure inside and add the clients that will be connected, no more and
no less. That's because the ToMCAT mission will start only when all the clients
listed in this file are connected.

## Create mission instance on server side

Server agent does not participate in TOMCAT mission, but it is needed to create
an mission instance on server side. We will hide server agent later. Use this
command: `./external/malmo/Minecraft/launchClient.sh -port 10000`. Here,
`-port` specifies the port number for the server, which should be the same as
the one in the configuration file.

## Create mission instances on client side

To run the mission on client machine, use this command:
`./external/malmo/Minecraft/launchClient.sh -port x`. Here, `-port` specifies
the port numbers for the clients. Those numbers need to be consistent with the
ones in the configuration file as well.

## Final step, start misison

After the server and all the client instances are ready, run this command on
server side to start your mission: 

```
./build/bin/runExperiment --mission 1 --multiplayer`.
```

The `--mission` flag specifies which tomcat mission will be running and
`--multiplayer` flag will treat the mission as a multiplayer mode and start the
game.
