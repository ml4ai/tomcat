# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/role_selected

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | the participant_id of the player being reported
| data.playername | string | The name of the entity that changed state
| data.new_role | string | The role that was switched to. From ENUM --> [None,Admin,Transport_Specialist,Engineering_Specialist,Medical_Specialist]
| data.prev_role | string | The role that was switched from. From ENUM --> [None,Admin,Transport_Specialist,Engineering_Specialist,Medical_Specialist]


## Message Example

```json
{
"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "1.0"
	},
"msg": {
	"experiment_id": "123e4567-e89b-12d3-a456-426655440000", 
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:RoleSelected",
	"version": "2.0"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"participant_id": "E000231",
	"playername":"Bulbousonions13",	
	"new_role": "Engineering_Specialist",	
	"prev_role": "None"
	}
}

```

VERSION | DATE | DETAILS
| --- | --- | --- |
2.0 | 11/1/2021 | change role type enums in data.new_role and data.prev_role key to [None,Admin,Transport_Specialist,Engineering_Specialist,Medical_Specialist]
1.0 | 3/10/2021 | Initital creation of schema
