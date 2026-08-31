# GroundTruth Subtype:Mission:VictimList Message Format
This GroundTruth message subtype is used to communicate the victim list.

## TOPIC

ground_truth/mission/victims_list

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.mission | string | The name of the mission in progress|
| data.mission_victim_list | List of mission_victim | the list of victims in the mission 

## Message Example

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "groundtruth",
		"version": "0.6"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Mission:VictimList",
		"version": "0.6"
	},
	"data": {
		"mission_timer":"Not Initialized",
		"mission": "SaturnA",	
		"mission_victim_list" : [ {
			"x" : -2088.0,
			"y" : 60.0,
			"z" : 146.0,
			"block_type" : "block_victim_1",
			"room_name" : "Security Office",
			"unique_id" : 1
		}, {
			"x" : -2087.0,
			"y" : 60.0,
			"z" : 146.0,
			"block_type" : "block_victim_2",
			"room_name" : "Security Office",
			"unique_id" : 2
		}, {
			"x" : -2086.0,
			"y" : 60.0,
			"z" : 146.0,
			"block_type" : "block_victim_saved",
			"room_name" : "Security Office",
			"unique_id" : 3
		} ]			
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
| 0.6 | 4/27/2021 | Included a victim_id for each victim. Changed all numbers to integers |
| 0.5 | NA | Initial Creation |