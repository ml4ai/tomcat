# Data Subtype: Status Message Format
This data message subtype is used to communicate the playerName to the gui.  
## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section 
| msg | object | From Common Event Message Format section 
| data.playername | string | the player name

## TOPIC
status/clientmapsystem/playername

## Message Example

```json
        {
            "header": { 
	            "timestamp": "2019-12-26T12:47:23.1234Z",
                "message_type": "status", 
                "version": "1.1" 
            }, 
            "msg": {  
                "experiment_id": "523e4567-e89b-12d3-a456-426655440000",
                "trial_id": "123e4567-e89b-12d3-a456-426655440000",
                "timestamp": "2019-12-26T14:05:02.1412Z",
                "source": "simulator", 
                "sub_type": "Status:PlayerName", 
                "version": "0.5" 
            },
            "data":{ 
                "playername":"Player123" 
            } 
        }'

```