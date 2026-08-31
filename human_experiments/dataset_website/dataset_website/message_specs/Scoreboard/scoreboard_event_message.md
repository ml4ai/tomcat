# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player.

## TOPIC

observations/events/scoreboard

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the event Header Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.scoreboard | object | The scoreboard represented as a JSON object with Player Names as keys and player scores as integer values


## Message Example

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "event",
		"version": "0.4"
	},
	"msg": { 
    	"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Event:Scoreboard",
		"version": "0.4"
	},
	"data": {
		"mission_timer":"8 : 36",
		"elapsed_milliseconds": 15113,
		"scoreboard": {
	        "Aptiminer": 10,
			"Player445": 40,
			"Guest2020": 10,
			"Player777": 90
	    }	
	}	
}

```