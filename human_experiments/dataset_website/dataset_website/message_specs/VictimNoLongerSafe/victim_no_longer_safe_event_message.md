# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/perturbation/victim_no_longer_safe

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.victim_x | integer | the x location of the victim
| data.victim_y | integer | the y location of the victim
| data.victim_z | integer | the z location of the victim
| data.color | string | the color of the victim [Yellow,Green]

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
	"sub_type": "Event:VictimNoLongerSafe",
	"version": "0.5"},
"data": {
	"mission_timer":"8 : 36",	
	"elapsed_milliseconds": 15113,	
	"color": "Green",	
	"victim_x": -2185,
	"victim_y":28,
	"victim_z":198
	}
}

```