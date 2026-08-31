# Header Message Type: Agent, Data Subtype: Version Info Message Format
This data message subtype is used to communicate information about a running agent in the testbed.

## TOPIC

agent/<unique_agent_name>/versioninfo

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.agent_name | string | the name of the agent being report on
| data.version | string | The version of the agent being reported on in the format <major>.<minor>.<patch>
| data.owner | string | the name of the person or organization that owns or supports this agent
| data.agent_type | string | the type of agent.  One of "ASI", "AC" or "other".  Only ASI agents can display messages to players.
| data.source | list of string | a list of the URLs of where the agent was obtained from
| data.dependencies | list of string | a list of the dependent components of the agent
| List of configuration parameters |  |
| data.config[n].name | string | The name of the configuration parameter
| data.config[n].value | string | The value of the configuration parameter 
| List of publishes | |
| data.publishes[n].topic | string | the topic the agent publishes the message on
| data.publishes[n].message_type | string | the message_type for the published message
| data.publishes[n].sub_type | string | the sub type of the message the agent publishes
| List of subscribes | |
| data.subscribes[n].topic | string | the topic the agent subscribes to
| data.subscribes[n].message_type | string | the message_type the agent subscribes to
| data.subscribes[n].sub_type | string | the message sub_type that the agent subscribes to

Version Numbering Guidence:
Agents should use the format of the version as shown above (<Major>,<Minor>.<Patch>).  General guidence for when to increment a part of the version should follow the Semantic Versioning Guidence.
- The agent should publish a different version identification whenever any change is made to the agent.
- The major number of the agent should track the major version of the testbed that it is designed to work with
- The minor number should be incremented when significant functionality is added to the agent.   This should also track incompatability between minor versions of the the agent
- The patch number should be incremented for each change to the agent

## Message Example

```json
{	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "agent",
		"version": "0.1"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "FoV",
		"sub_type": "versioninfo",
		"version": "0.1"
	},
	"data": {
		"agent_name":"FoV",
		"version": "1.2.1",
		"owner": "My Organization",
		"agent_type": "ASI",
		"config": [{"name": "model_name", "value": "multi-player"},
						{"name": "loop_back_depth", "value": "3"}
		],
		"source": ["https://registry.gitlab.com/artificialsocialintelligence/study3/pygl_fov_agent:2.0.0-dev.349"],
		"dependencies": ["python package1", "python package2", "python package3"],
		"publishes": [{"topic": "agent/fov", "message_type": "agent", "sub_type": "fov"}],
		"subscribes": [{"topic": "observations/state", "message_type": "observation", "sub_type": "state"},
			{"topic": "observations/events/player/lever", "message_type": "event", "sub_type": "Event:Lever"},
			{"topic": "observations/events/player/door", "message_type": "event", "sub_type": "Event:Door"},
			{"topic": "observations/events/mission", "message_type": "event", "sub_type": "Event:MissionState"},
			{"topic": "ground_truth/mission/victims_list", "message_type": "groundtruth", "sub_type": "Mission:VictimList"},
			{"topic": "ground_truth/mission/blockages_list", "message_type": "groundtruth", "sub_type": "Mission:BlockageList"}]
	}
}

```
