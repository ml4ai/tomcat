# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player.

## TOPIC

observations/events/competency/task

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | the participant id for the player being reported
| data.callsign | string | the callsign of the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state| 
| task_message | string | The message indicating the competency task status


## Message Example

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "event",
		"version": "1"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Event:CompetencyTask",
		"version": "2.0"
	},
	"data": {
	    "mission_timer":"8 : 36",
		"elapsed_milliseconds": 15113,
		"playerName": "Player682",
		"participant_id": "P101",
		"callSign": "Green",
		"task_message": "Green Training: Task #1 Complete"
  }
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
2.0 | 1/17/2021 | Schema updated to differentiate between training and competency tasks - similar schema exists for training tasks under TrainingTask folder. Added various enums for the task messages as well as standard player id fields and timestamps. |
1.0 | ?/?/2020 | Initial Creation |