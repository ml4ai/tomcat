# Trial Message Format
A message giving the summary of the text displayed to the participants for the puzzle.  

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| mission_name | string | the mission name
| medical_specialist_puzzle_summary | string[] | summary of text to be displayed on the medic client map
| transport_specialist_puzzle_summary | string[] | summary of text to be displayed on the transporter client map
| engineering_specialist_puzzle_summary | string[] | summary of text to be displayed on the engineer client map

## TOPIC
observations/events/mission/puzzle_summary

## Message Example

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "event",
		"version": "0.6"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Mission:PuzzleTextSummary",
		"version": "2.0"
	},
	"data": {

		"mission_name": "Saturn_A",
		"medical_specialist_puzzle_summary": ["MEETING LOCATIONS"],
		"transport_specialist_puzzle_summary": [,"MEETING ATTENDANTS"],
		"engineering_specialist_puzzle_summary": ["ROOM DAMAGE SEVERITY"]        
    
	}
}

```

VERSION | DATE | DETAILS
| --- | --- | --- |
| 2.0 | 03/10/22 | Initial Spec Created