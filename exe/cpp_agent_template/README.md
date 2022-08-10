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
  "agent_input": {
    "message_type": "chat",
    "sub_type": "Event:Chat",
    "topic": "minecraft/chat"
  },
  "agent_output": {
    "message_type": "agent",
    "sub_type": "template_agent",
    "topic": "agent/template_agent_output"
  },
  "heartbeat": {
    "message_type": "status",
    "sub_type": "heartbeat",
    "topic": "status/template_agent/heartbeats"
  },
  "rollcall_request": {
    "message_type": "agent",
    "sub_type": "rollcall:request",
    "topic": "agent/control/rollcall/request"
  },
  "rollcall_response": {
    "message_type": "agent",
    "sub_type": "rollcall:response",
    "topic": "agent/control/rollcall/response"
  },
  "trial_start": {
    "message_type": "trial",
    "sub_type": "start",
    "topic": "trial"
  },
  "trial_stop": {
    "message_type": "trial",
    "sub_type": "stop",
    "topic": "trial"
  },
  "version_info": {
    "message_type": "agent",
    "sub_type": "versioninfo",
    "topic": "agent/template_agent/versioninfo"
  },
  "version": "1.0.0",
  "notes": "Preliminary C++ template agent",
  "publication_source": "template_agent"
}
```

If a field in the config file is also specified on the command line, the command line value is used.
