# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/rubble_destroyed

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | the participant id for the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state
| data.rubble_x | integer | the x location of the rubble
| data.rubble_y | integer | the y location of the rubble
| data.rubble_z | integer | the z location of the rubble


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
	"sub_type": "Event:RubbleDestroyed",
	"version": "1.1"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"participant_id":"E000321",
	"rubble_x": -2185,
	"rubble_y":28,
	"rubble_z":198
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS

1.1 | 8/13/2021 | deprecated playername and added participant_id

