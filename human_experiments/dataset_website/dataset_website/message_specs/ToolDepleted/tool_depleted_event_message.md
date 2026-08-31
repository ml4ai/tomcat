# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/tool_depleted

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | the participant id of the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state
| data.tool_type | string | The type of tool being used. From ENUM --> [ "MEDKIT", "STRETCHER","STRETCHER_OCCUPIED","HAMMER","NULL"]

## Message Example

```json
{
"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "1.1"
	},
"msg": {
	"experiment_id": "123e4567-e89b-12d3-a456-426655440000", 
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:ToolDepleted",
	"version": "1.0"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"participant_id": "E000321",	
	"tool_type": "MEDKIT"
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS

1.1 | 8/13/2021 | deprecated playername and added participant_id

1.0 | 3/5/2021 | Standardized tool_type enum field to Upper Case tools for all 3 roles. 

0.5 | 1/26/2021 | Initial state when ported from ADAPT project
