# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/freeze

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.state_changed_to | string enum | FROZEN - player cannot move and is somewhat blinded, UNFROZEN - player was helped by a teammate who was assigned the Medic Role and can now move and see normally
| data.participant_id | string | the participant_id of the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state| 
| data.player_x | integer | the x location of the player
| data.player_y | integer | the y location of the player
| data.player_z | integer | the z location of the player
| data.medic_participant_id | string | the participant_id of the medic that facilitated the transition to UNFROZEN state
| data.medic_playername| string | [Deprecated]The name of the medic that fascilitated the tranzition to UNFROZEN state|


## Message Examples

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "event",
		"version": "0.5"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Event:PlayerFrozenStateChange",
		"version": "1.0"
	},
	"data": {
		"mission_timer":"8 : 36",
		"elapsed_milliseconds": 384111,
		"participant_id": "E000213",	
		"state_changed_to": "FROZEN",		
		"victim_x": -2185,
		"victim_y":28,
		"victim_z":198,			
	}
}

```

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "event",
		"version": "0.5"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Event:PlayerFrozenStateChange",
		"version": "1.0"
	},
	"data": {
		"mission_timer":"8 : 36",
		"elapsed_milliseconds": 384111,
		"participant_id": "E000231",	
		"state_changed_to": "UNFROZEN",		
		"victim_x": -2185,
		"victim_y":28,
		"victim_z":198,	
		"medic_participant_id":"E000321"		
	}
}

```
