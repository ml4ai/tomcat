# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/server/rubble_placed

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.from_x | integer | the starting x location of the rubble
| data.from_y | integer | the starting y location of the rubble
| data.from_z | integer | the starting z location of the rubble
| data.to_x | integer | the ending x location of the rubble
| data.to_y | integer | the ending y location of the rubble
| data.to_z | integer | the ending z location of the rubble


## Message Example

```json
{"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "1.0"
	},
"msg": {
	"experiment_id": "123e4567-e89b-12d3-a456-426655440000", 
    	"trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:RubblePlaced",
	"version": "0.5"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"from_x": -2185,
	"from_y":28,
	"from_z":198,	
	"to_x": -2285,
	"to_y":38,
	"to_z":158	
	}
}

```