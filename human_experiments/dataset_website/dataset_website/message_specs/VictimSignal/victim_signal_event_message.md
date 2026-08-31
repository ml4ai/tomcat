# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player.  
## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section 
| msg | object | From Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.playername | string | The name of the player triggering the Victim Signal Event
| data.message | string | the message emitted by the signal device
| data.x | integer | the x location of the entity
| data.y | integer | the y location of the entity
| data.z | integer | the z location of the entity
| data.roomname | string | the roomname associated with this signal


## TOPIC
observations/events/player/signal

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
	"sub_type": "Event:Signal",
	"version": "2.0"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"playername": "Aptiminer1",
	"participant_id": "P100",	
	"message": "Regular Victim Detected",	
	"x": -2185,
	"y":28,
	"z":198,
	"roomname":"A2"		
	}
}

```

VERSION | DATE | DETAILS
| --- | --- | --- |
2.1 | 12/21/2021 | updated message enums to ["No Victim Detected","Regular Victim Detected", "Critical_Victim_Detected"]
2.0 | 11/23/2021 | updated message from Beep to VictimSignal with new properties -> roomname, participant_id