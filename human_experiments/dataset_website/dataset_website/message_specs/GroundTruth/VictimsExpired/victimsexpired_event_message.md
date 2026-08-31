# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player.

## TOPIC

ground_truth/mission/victims_expired

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| expired_message | string | The message indicating all yellow victims have expired


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
		"sub_type": "Event:VictimsExpired",
		"version": "0.5"
	},
	"data": {
		"mission_timer":"8 : 36",
		"expired_message":"All remaining yellow victims have succumbed to their injuries."	
		
	}
}

```