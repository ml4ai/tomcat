# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

NOTE: THIS MESSAGE IS NOT USED IN THE CURRENT ASIST 3.0 EXPERIMENT - MARKER BLOCKS ARE NEVER DESTROYED ONLY REPLACED AND REMOVED. SEE MarkerRemoved message.

## TOPIC

observations/events/perturbation/marker_destroyed

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.marker_x | integer | the x location of the marker
| data.marker_y | integer | the y location of the marker
| data.marker_z | integer | the z location of the marker
| data.type | string | the type of the marker  [Marker Block 1, Marker Block 2, Marker Block 3,Marker Block 4,Marker Block 5,Marker Block 6]

## Message Example

```json
{"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "1.0"
	},
"msg": {
	"experiment_id": "123e4567-e89b-12d3-a456-426655440000", 
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:MarkerDestroyed",
	"version": "0.5"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,	
	"type": "red_abrasion",	
	"marker_x": -2185,
	"marker_y":28,
	"marker_z":198,	
	}
}

```

VERSION | DATE | DETAILS
| --- | --- | --- |
2.1 | 11/19/2021 | updated marker block enums --> removed [critical,wildcard] replaced with [novictim,sos] for red,green,blue variations
2.0 | 11/1/2021 | updated marker block enums to the following list:
[red_abrasion,red_bonedamage,red_critical,red_regularvictim,red_criticalvictim,red_rubble,red_threat,red_wildcard,green_abrasion,green_bonedamage,green_critical,green_regularvictim,green_criticalvictim,green_rubble,green_threat,green_wildcard,blue_abrasion,blue_bonedamage,blue_critical,blue_regularvictim,blue_criticalvictim,blue_rubble,blue_threat,blue_wildcard]
1.0 | 3/10/2021 | Initial Schema creation
         