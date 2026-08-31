# The ASIST user speech message Schema
This message includes the text results of speech to text translation of user utterances.

## TOPIC
status/asistdataingester/userspeech

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section
| data.playername | string | The player name of the user who spoke the utterance
| data.text | string | The text of the utterance

## Message Examples
```json
{ "header": {
    "timestamp": "2021-02-10T14:05:02.3412Z",
    "message_type": "status",
    "version": "0.1"
    },
  "msg": { 
    "experiment_id": "523e4567-e89b-12d3-a456-426655440000",
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2021-02-10T14:05:02.3412Z",
    "source": "asistdataingester",
    "sub_type": "Status:UserSpeech",
    "version": "0.4"},    
  "data": {
    "playername": "Aptiminer",
    "text": "I am using Minecraft"
  }
}
```
