# Data Subtype: Chat Message Format
This data message subtype is used to communicate chat information from a human or agent to any component on the message bus that is interested in it. 

## TOPIC

chat

## Message Fields

| Field Name | Type | Description
| --- | --- | --- |
| header | object | From Common Message Format section
| msg.trial_id | string | The experiment trial id associated with this chat message
| msg.replay_id | string | If the original trial data was replayed, this field indicates a unique uuid for the replay
| msg.timestamp | string | Timestamp of when the data was generated in ISO 8601 format: YYYY-MM-DDThh:mm:ss.ssssZ
| msg.source | string | The name of the testbed component that published this data
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.sender | string | the name of the sender
| data.addressees | [string] | an arrary of addressee names
| data.text | string | the text of the chat message


## Message Example(s)

```json
{ 	
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "chat",
		"version": "1.1"
	},
	"msg": { 
		"experiment_id":"123e4567-e89b-12d3-a456-426655440000",
  		"trial_id": "123e4567-e89b-12d3-a456-426655440000",		
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Event:Chat",
		"version": "0.5", 	
	},
	"data":{
		"mission_timer":"8 : 36",
		"elapsed_milliseconds": 15113,
		"sender":"Aptiminer1",
		"addressees": ["Player746"],
		"text": "I'm in room 210"
	}
}

{ 
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "chat",
		"version": "1.1"
	},
	"msg": { 
		"experiment_id":"123e4567-e89b-12d3-a456-426655440000",
  		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"replay_id": "876e4567-ab65-cfe7-b208-426305dc1234",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Event:Chat",
		"version": "1.0", 		
	},
	"data":{
		"mission_timer":"8 : 36",
		"sender":"Aptiminer1",
		"addressees": ["Player746"],
		"text": "I'm in room 210"
	}
}
```

## CHANGE HISTORY

VERSION | DATE | DETAILS

1.0 | 3/1/2021 | Added elapsed_milliseconds field 


