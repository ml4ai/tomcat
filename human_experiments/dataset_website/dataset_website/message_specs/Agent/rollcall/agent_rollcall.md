# Header Message Type: Agent, Data Subtype: rollcall:request, rollcall:response Message Format
This data message subtype is used to conduct a rollcall on all operating agents in a running testbed.

## TOPIC

agent/control/rollcall/[request | response]

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| msg.source | string | for request, the identity that is making the request, for response, the identity that is responding
| data.rollcall_id | string | the id of the rollcall request.
| data.version | string | The version of the agent being reported on in the format <major>.<minor>.<patch>
| data.status | string | the current status of the agent.  One of ["initializing" | "up" | "unknown" | "down"]
| data.uptime | integer | The number of seconds since the agent was started up
| data.agent_type | string | the type of the agent.  One of "ASI", "AC", "other".  Only ASI agents are allowed to generate intervention messages that can be displayed to the players.


## Message Example - Request

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
		"source": "testbed",
		"sub_type": "rollcall:request",
		"version": "0.1"
	},
	"data": {
		"rollcall_id": "385e4567-e49b-12e3-a456-426655440132"
	}
}
```
## Message Example = Reponse
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
		"source": "aptima_reference_agent",
		"sub_type": "rollcall:response",
		"version": "0.1"
	},
	"data": {
		"rollcall_id":"385e4567-e49b-12e3-a456-426655440132",
		"version": "1.2.1",
		"status": "up",
		"uptime": 780,
		"agent_type": "ASI"
	}
}
```
VERSION | DATE | DETAILS
| --- | --- | --- | 
| 0.1 | 11/1/2021 | Initial Spec Created |