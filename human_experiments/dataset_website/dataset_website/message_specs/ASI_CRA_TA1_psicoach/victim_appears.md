# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

agent/psicoach/victim_appears

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.SecondaryEvent | string | For internal use only
| data.location | string | the location of the victim
| data.missionTime | string | the mission time of the event
| data.timestamp | string | The system time of the event
| data.victim | string | the ID of the victim

## Message Example

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "event",
		"version": "1.1"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "ASI_CRA_TA1_psicoach",
		"sub_type": "Event:VictimAppears",
		"version": "1.1"
	},
	"data": {
		"SecondaryEvent": "VictimAppearsEvent",	
		"location":"(0.7, 2.6, 0.8)",
		"missionTime": 600,
		"timestamp": "2019-12-26T14:05:02.890Z",
		"victim": "VictimID[2080404575]"
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
