# Data Subtype: Player State Message Format
This data message subtype is used to communicate player location and state information from the simulator to any component on the message bus that is interested in it. 

## TOPIC

observations/state

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.observation_number | integer | A sequence number of the number of observations from the beginning of the trial run
| data.timestamp | string | timestamp of when the data was captured. UTC timezone formatted as ISO 8601: YYYY-MM-DDThh:mm:ss.ssssZ
| data.world_time | integer | Current time in ticks
| data.total_time | integer | Total world time, unaffected by ServerInitialConditions
| data.entity_type | string | The type of the entity e.g. "human"
| data.yaw | float | The current yaw of the entity's direction. Yaw has a range -360 to +360.
| data.x | float | The current x location of the entity
| data.y | float | The current y location of the entity
| data.z | float | The current z location of the entity
| data.pitch | float | The  current pitch of the entity's direction
| data.id | string | A UUID format unique identifier for the entity 
| data.motion_x | float | The x direction motion of the entity. The delta change in position since the last observation in the x direction.
| data.motion_y | float | The y direction motion of the entity. The delta change in position since the last observation in the y direction. 
| data.motion_z | float | The z direction motion of the entity. The delta change in position since the last observation in the z direction.
| data.life | float | The current life value for the entity
| data.playername | string | [Deprecated]The name of the entity
| data.participant_id | string | the participant id of the player being reported


## Message Example

```json
{"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "observation",
	"version": "0.4"
	},
"msg": { 
	"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
	"trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "state",
	"version": "1.1"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"observation_number":6857,
	"timestamp":"2019-12-18T19:13:46.1452Z",
	"world_time":12000,
	"total_time":6706346,
	"entity_type": "human",
	"yaw":19.6875,
	"x":-2198.91357421875,
	"y":23.0,
	"z":193.04052734375,
	"pitch":4.21875,
	"id":"699ba7c4-ceaf-3c46-a3cf-50b02c2ff935",
	"motion_x":0.08991309562755659,
	"motion_y":0.0,
	"motion_z":-0.11381933207744127,
	"life":20.0,
	"participant_id":"E000324"
	}
}

```
## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
| 1.1 | 8/13/2021 | deprecated playername and added participant_id |
