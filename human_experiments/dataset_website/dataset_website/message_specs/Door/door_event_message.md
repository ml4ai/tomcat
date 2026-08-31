# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/door

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | The participant id of the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state
| data.door_x | integer | the x location of the entity
| data.door_y | integer | the y location of the entity
| data.door_z | integer | the z location of the entity
| data.open | boolean | the new state of the entity

## Message Example

```json
{"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "0.4"
	},
"msg": {
	"experiment_id": "123e4567-e89b-12d3-a456-426655440000", 
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:Door",
	"version": "0.4"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"participant_id": "E000213",	
	"open": true,	
	"door_x": -2185,
	"door_y":28,
	"door_z":198,	
	}
}

```

## Version Change History
| Version | Date | Description | 
| --- | --- | --- |
| 1.2 | 8/12/2021 | added participant_id and made playername optional and deprecated |
| 1.1 | 3/1/2021 | Added elapsed_milliseconds field |
| 1.0 | 6/30/2021 | Initial version |
