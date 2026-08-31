# Event Subtype:Event:PerturbationRubbleLocations Message Format
This message is used to communicate the locations of the individual rubble blocks placed during the rubble perturbation.

## TOPIC

observations/events/player/perturbation_rubble_locations

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.mission | string | The name of the mission in progress|
| data.mission_blockage_list | Array of type BlockageList | an array of all blockages placed in the perturbation

## Message Example

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
		"sub_type": "Mission:PerturbationRubbleLocations",
		"version": "2.1"
	},
	"data": {
		"mission_timer":"8 : 36",
		"elapsed_milliseconds": 15113,
		"mission": "Saturn_A_Rubble",	
	   	"mission_blockage_list" : [ {
			"x" : -2088.0,
			"y" : 60.0,
			"z" : 146.0,
			"block_type" : "gravel",
			"room_name" : "",
			"feature_type" : "obstruction"
		}, {
			"x" : -2087.0,
			"y" : 60.0,
			"z" : 146.0,
			"block_type" : "gravel",
			"room_name" : "",
			"feature_type" : "obstruction"
		} ]
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
2.0 | 12/23/2021 | Initial spec created