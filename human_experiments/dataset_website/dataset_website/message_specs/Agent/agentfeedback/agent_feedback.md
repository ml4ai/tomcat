# Header Message Type: Agent, Data Subtype: agent:feedbacktoagent Message Format
This data message subtype is used to communicate feedback from the player/subject to the operating agents in a running testbed.

## TOPIC

agent/control/feedbacktoagent

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| msg.source | string | indicates the source of this message is a player.  It is possible in the future that it might be source from other places such as other agents.
| data.participant_id | string | the id of the participant providing the feedback.
| data.feedback_type | string | enumeration of feedback types ["text" | "image"]  image is a future type
| data.feedback_text | string | the content of the feedback, if the feedback_type is "text"


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
		"source": "player",
		"sub_type": "agent:feedbacktoagent",
		"version": "0.1"
	},
	"data": {
		"participant_id": "P000213",
		"feedback_type": "text",
		"feedback_text": "I don't trust your advice"
	}
}
```