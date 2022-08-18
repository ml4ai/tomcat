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
  "agent_name": "AC_UAZ_TA1_ReferenceAgent",
  "owner": "University of Arizona",
  "version": "1.0.0",
  "config": [],
  "dependencies": [],
  "source": [
    "https://gitlab.asist.aptima.com:5050/asist/testbed/AC_UAZ_TA1_ReferenceAgent:i.j.k"
  ],
  "publishes": [
    {
      "topic": "agent/reference_agent_output_1",
      "message_type": "reference_agent_output_message_type_1",
      "sub_type": "reference_agent_output_sub_type_1"
    },
    {
      "topic": "agent/reference_agent_output_2",
      "message_type": "reference_agent_output_message_type_2",
      "sub_type": "reference_agent_output_sub_type_2"
    }
  ],
  "subscribes": [
    {
      "topic": "agent/reference_agent_input_1",
      "message_type": "reference_agent_input_message_type_1",
      "sub_type": "reference_agent_input_sub_type_1"
    },
    {
      "topic": "agent/reference_agent_input_2",
      "message_type": "reference_agent_input_message_type_2",
      "sub_type": "reference_agent_input_sub_type_2"
    }
  ]
}
```

If a field in the config file is also specified on the command line, the command line value is used.
