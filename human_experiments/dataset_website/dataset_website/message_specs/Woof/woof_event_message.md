# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/woof

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section 
| msg | object | From Common Event Message Format section 
| data.source_entity | string | The name of the entity triggering the Woof Event
| data.message | string | the message emitted by the source_entity
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.woof_x | integer | the x location of the entity
| data.woof_y | integer | the y location of the entity
| data.woof_z | integer | the z location of the entity


## Message Example

```json
{"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "0.5"
	},
"msg": { 
	"experiment_id": "523e4567-e89b-12d3-a456-426655440000",
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:Woof",
	"version": "0.5"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"source_entity": "Search and Rescue Dog",	
	"message": "woof",	
	"lever_x": -2185,
	"lever_y":28,
	"lever_z":198,	
	}
}

```