# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/triage

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | the participant id for the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state| 
| data.victim_x | integer | the x location of the entity
| data.victim_y | integer | the y location of the entity
| data.victim_z | integer | the z location of the entity
| data.type | string | the type of the victim being triaged [victim_a,victim_b,victim_c]
| data.triage_state | string | the new state of the triage event - IN_PROGRESS on MOUSEDOWN and first visible damage, UNSUCCESSFUL on MOUSEUP if victim not yet fully triaged, SUCCESSFUL on full triage
| data.victim_id | integer | the unique id identifying the victim

## Message Example

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "event",
		"version": "0.6"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Event:Triage",
		"version": "2.0"
	},
	"data": {
		"mission_timer":"8 : 36",
		"elapsed_milliseconds": 15113,
		"participant_id": "E000321",
		"triage_state": "IN_PROGRESS",	
		"victim_x": -2185,
		"victim_y":28,
		"victim_z":198,
		"type":"victim_a",
		"victim_id":33	
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
2.0 | 11/1/2021 | change victim type identifier in data.type key to [victim_a,victim_b,victim_c]
1.3 | 8/13/2021 | deprecated playername and added participant_id
1.2 | 4/27/2021 | Added victim_id field
1.1 | 4/12/2021 | data.color changed to data.type with types [REGULAR,CRITICAL]
1.0 | 3/10/2021 | data.color is now upper case with only GREEN and YELLOW as options