# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/rubble_collapse

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | the participant id for the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state
| data.triggerLocation_x | integer | the x location of the trigger block
| data.triggerLocation_y | integer | the y location of the trigger block
| data.triggerLocation_z | integer | the z location of the trigger block
| data.fromBlock_x | integer | the x location of one corner defining a rectangle of rubble
| data.fromBlock_y | integer | the y location of one corner defining a rectangle of rubble
| data.fromBlock_z | integer | the z location of one corner defining a rectangle of rubble
| data.toBlock_x | integer | the x location of the other corner defining a rectangle of rubble
| data.toBlock_y | integer | the y location of the other corner defining a rectangle of rubblek
| data.toBlockn_z | integer | the z location of the other corner defining a rectangle of rubble


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
	"sub_type": "Event:RubbleCollapse",
	"version": "2.1"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"participant_id": "E000321",
	"triggerLocation_x": -2100,
	"triggerLocation_y": 59,
	"triggerLocation_z": 61,
	"fromBlock_x": -2101,
	"fromBlock_y": 60,
	"fromBlock_z": 61,
	"toBlock_x": -2099,
	"toBlock_y": 59,
	"toBlock_z": 64	
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
2.1 | 11/1/2021 | Topic change observations/events/server/rubble_collapse --> observations/events/player/rubble_collapse
2.0 | 11/1/2021 | Initial schema creation
