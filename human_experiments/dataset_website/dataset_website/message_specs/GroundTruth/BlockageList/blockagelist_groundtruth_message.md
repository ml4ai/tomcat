# GroundTruth Subtype:Mission:BlockageList Message Format
This GroundTruth message subtype is used to communicate the blockage list.

## TOPIC

ground_truth/mission/blockages_list

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission | string | The name of the mission in progress|
| data.mission_blockage_list | List of mission_blockage | the list of blockages in the mission 

## Message Example

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "groundtruth",
		"version": "0.5"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Mission:BlockageList",
		"version": "0.5"
	},
	"data": {
		"mission": "Falcon",	
	   	"mission_blockage_list" : [ {
			"x" : -2088.0,
			"y" : 60.0,
			"z" : 146.0,
			"block_type" : "cobblestone",
			"room_name" : "",
			"feature_type" : "Blockage"
		}, {
			"x" : -2087.0,
			"y" : 60.0,
			"z" : 146.0,
			"block_type" : "dirt",
			"room_name" : "",
			"feature_type" : "Blockage"
		} ]
	}
}

```