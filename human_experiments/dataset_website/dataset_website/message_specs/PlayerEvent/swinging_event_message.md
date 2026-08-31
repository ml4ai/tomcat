# Data Subtype: Event:PlayerSwinging Message Format
This data message subtype is used to communicate when the player swings

## TOPIC

observations/events/player/swinging

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | The participant id of the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state|
| data.swinging | boolean | Is the player swinging

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
	"sub_type": "Event:PlayerSwinging",
	"version": "0.4"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"participant_id": "E000213",	
	"swinging" : true
	}
}

```
