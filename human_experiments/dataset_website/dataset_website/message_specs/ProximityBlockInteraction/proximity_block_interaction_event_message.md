# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/proximity_block

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.action_type | string enum | ENTERED_RANGE - players has entered triage range, LEFT_RANGE - player has left triage range, TRIAGE_ERROR - player has attempted triage before all required players have assembled
| data.participant_id | string | the participant id of the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state| 
| players_in_range | number | the number of players that are within range of the victim
| data.victim_x | integer | the x location of the entity
| data.victim_y | integer | the y location of the entity
| data.victim_z | integer | the z location of the entity
| data.color | string | the color of the victim being triaged
| data.triage_state | string | the new state of the triage event - IN_PROGRESS on MOUSEDOWN, UNSUCCESSFUL on MOUSEUP if victim not yet fully triaged, SUCCESSFUL on full triage
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
		"sub_type": "Event:ProximityBlockInteraction",
		"version": "2.1"
	},
	"data": {
		"mission_timer":"8 : 36",
		"participant_id": "E000231",
		"action_type": "ENTERED_RANGE",
		"elapsed_milliseconds": 15113,
		"players_in_range":2,
		"awake":false,
		"victim_x": -2185,
		"victim_y":28,
		"victim_z":198,
		"victim_id":33
	}
}

```
## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
2.1 | 3/04/2022 | Added "awake" key to data. Will be true when the victim wakes up due to 1 medic and + n required teammates present
2.0 | 12/20/2021 | Changed Event:ProximityVictimInteraction to Event:ProximityBlockInteraction and topic to "observations/events/player/proximity_block"
1.2 | 8/12/2021 | deprecated playername and added participant_id
1.1 | 4/27/2021 | Added victim_id field
1.0 | 3/1/2021 | Initial Creation
