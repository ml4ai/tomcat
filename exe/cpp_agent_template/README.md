# ToMCAT C++ Agent Template

A framework for rapid development of Message Bus Agents in C++.

This framework handles those message bus operations that are common to all agents, allowing the developer to focus solely on the  
input processing and output generation specific to their agent.  

This software does not publish null JSON values.

### Requirements
You must have Boost version 1.75 or later installed for the JSON library.

### Building the agent

```
mkdir build
cd build
cmake ..
make -j
```

To run the agent (assuming you are in the build directory)

```
./main
```


## Configuration

The following options and settings are available on the command line:

```
Configuration:
  -h [ --help ]                         Display this help message
  -v [ --version ]                      Display the version number
  -c [ --config ] arg (=../config.json) Specify a config file
  --mqtt.host arg (=localhost)          MQTT broker host
  --mqtt.port arg (=1883)               MQTT broker port
```

Additional settings are made in a JSON configuration file:

```
{
  "mqtt": {
    "host": "localhost",
    "port": 1883
  },
  "publications": {
    "heartbeats": "status/template_agent/heartbeats",
    "agent_output": "agent/template_agent_output",
    "rollcall_response": "agent/control/rollcall/response",
    "version_info": "agent/template_agent/versioninfo"
  },
  "subscriptions": {
    "agent_input": "agent/template_agent_input",
    "rollcall_request": "agent/control/rollcall/request",
    "trial": "trial"
  },
  "version":"1.0.0"
}
```

If a field in the config file is also specified on the command line, the command line value is used.
