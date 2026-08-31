# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/server/victim_evacuated

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | the participant id for the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state
| data.victim_x | integer | the x location of the victim
| data.victim_x | integer | the y location of the victim
| data.victim_x | integer | the z location of the victim
| data.type | string | the type of the victim being triaged [victim_a,victim_b,victim_saved_a,victim_saved_b,victim_saved_c]
| data.victim_id | integer | the unique id identifying the victim
| data.success | boolean | whether or not the victim was evacuated to the area corresponding with its letter identifier (a,b,c)

## Message Example

```json
{"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "0.6"
	},
"msg": {
	"experiment_id": "123e4567-e89b-12d3-a456-426655440000", 
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:VictimEvacuated",
	"version": "2.2"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"participant_id": "E000321",
	"type":"victim_a_saved",	
	"victim_x": -2185,
	"victim_y":28,
	"victim_z":198,
	"victim_id":33,
	"success":true
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
2.2 | 04/29/2022 | Added [victim_a,victim_b] to possible types of victims evacuated ("success" :false because not triaged)
2.1 | 11/11/2021 | changed victim_safe_a/b/c to victim_saved_a/b/c as in the mod [victim_saved_a,victim_saved_b,victim_saved_c]
2.0 | 11/1/2021 | Initial schema creation
