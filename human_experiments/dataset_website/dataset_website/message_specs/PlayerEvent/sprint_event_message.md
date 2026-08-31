# Data Subtype: Event:PlayerSprinting Message Format
This data message subtype is used to communicate when the player sprints

## TOPIC

observations/events/player/sprinting

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section
| data.mission_timer | string | the mission time of the event 
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.playername | string | The name of the entity that changed state|
| data.sprinting | boolean | Is the player sprinting

## Message Example

```json
{"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "0.4"
	},
"msg": { 
	"experiment_id": "523e4567-e89b-12d3-a456-426655440000",
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:PlayerSprinting",
	"version": "0.4"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"playername": "Aptiminier1",	
	"sprinting" : true
	}
}

```