# Trial Message Format
A trial message describes the characteristics of a trial.  

## TOPIC

control/response/getTrialInfo


## Message Fields

| Field Name | Type | Description|
 --- | --- | ---

| trial_id | string | The trial id.
| experiment_id | string | The experiment id.
| mission_name | string | The mission name

## Message Examples
```json
{ 
	"experiment_id":"946d4567-ab65-cfe7-b208-426305dc1234",
    "trial_id":"85ed4567-ab65-cfe7-b208-426305dc1234",
   	"mission_name": "Saturn_A",
	"map_name":"Saturn_1.1_3D",
	"map_block_filename":"MapBlocks_SaturnA_1.3_xyz.csv",
	"map_info_filename":null,
	"observer_info":[],
	"callsigns":{
		"Player239":"Red",
		"Aptiminer1":"Blue",
		"SomeDude44":"Green"
	},
	"participant_ids":{
		"Player239":"P100",
		"Aptiminer1":"P101",
		"SomeDude44":"P102"
	}
}
```

## Version Change History
| Version | Date | Description | 
| --- | --- | --- |
2.1 | 12/07/2021 | Added "active_agents" key for agent filtering in Minecraft
2.0 | 11/12/2021 | Flipped key-values in callsigns and participant_ids so as to support more flexibility in mod should callsigns/pids format change. Added participant_ids documentation.
1.0 | 6/30/2021 | Initial version
