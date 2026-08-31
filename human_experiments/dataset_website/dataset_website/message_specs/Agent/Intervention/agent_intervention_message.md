# Agent Intervention Message Format
An Agent Intevention Message is used to communicate information from an agent to a Minecraft Player during a mission. 

All agents communication should adhere to the agent_intervention_message.json schema
## TOPIC

agent/intervention/<unique_agent_name>/chat
agent/intervention/<unique_agent_name>/block
agent/intervention/<unique_agent_name>/map

## Message Fields

| Field Name | Type | Description
| --- | --- | --- |
| header | object | See the Common Header Directory
| msg | object | See the Common Message Directory
| msg.sub_type | Intervention:Chat, Intervention:Block, Intervention:Map | This subtype is used to issue a chat, block or map intervention to a Minecraft Player
| msg.source | string | the name of the agent that generated the intervention
| data.id | id | a unique id for the interventions
| data.created | string timestamp | the time when this intervention was created in a ZULU/UCT time string
| data.start | integer | The time the prediction/intervention is effective. Specified in mission elapsed time in milliseconds.  If the start time is a number less then zero like -1, the intervention should start immediately.
| data.duration | integer | The length of time in milliseconds the prediction/intervention remains valid. In the case of a block, this is the duration the block should remain in the world before being removed
| data.explanation | json object | this field contains what ever the agent wants to include with this specific intervention that can be used for post experiment analysis.

### FOR CHAT INTERVENTIONS

NOTE* CURRENTLY ONLY STRING CHAT INTERVENTIONS ARE IMPLEMENTED. TO FIRE OFF AN INTERVENTION IMMEDIATELY WITHOUT WAITING FOR IT TO QUEUE,
SET THE THE START TIME TO ANY VALUE LESS THAT 0 ... IE "start":-1


| Field Name | Type | Description
| --- | --- | --- |
| data.content | string | The message from the agent to display to the player/user if using Intervention:Chat 
| data.receivers | list[string] | The participant ids who should receive the chat message.  Each participant id in the list will receive the intervention message.
| data.type | enum | Type of data to be displayed. ["json", "string", "HTML", "block"]
| data.renderers | list[string] |  The renderer used to display the content. [ "Minecraft_Chat", "Minecraft_Block", "Client_Map" ]

### FOR BLOCK INTERVENTIONS

NOTE* NOT CURRENTLY IMPLEMENTED

| Field Name | Type | Description
| --- | --- | --- |
| data.interventions[n].block_type | string | The type of the block to be installed.  Could use original to replace the block with the original block that existed at that location
| data.interventions[n].block_x | number | The x coordinates of where the block should be placed
| data.interventions[n].block_y | number | The y coordinates of where the block should be placed
| data.interventions[n].block_z | number | The z coordinates of where the block should be placed
| data.renderers | list[string] |  The renderer used to display the block content. [ "Minecraft_Block", "Client_Map" ]

### FOR MAP INTERVENSIONS - Future

NOTE* NOT CURRENTLY IMPLEMENTED

| Field Name | Type | Description
| --- | --- | --- |
| data.map | URL | The URL of the map to be displayed
| data.annotation[n].type | string | The type of an annotation e.g. line, text, circle
| data.annotation[n].color | string | the color of the annotation
| data.annotation[n]. | TBD | other parameters depending on the annotation type such as the end points of the line, the text and text location, the center of the circle and the radius

## Message Example

```json
{ 
  "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "agent",
    "version": "0.1"
  },
  "msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2019-12-26T14:05:02.1412Z",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "replay_root_id": "123e4567-e89b-12d3-a456-426655440000",
    "source": "tom_generator:1.0",
    "sub_type": "Intervention:Chat",
    "version": "0.1"
  },
  "data": {
      "id": "<uuid for the intervention>",
      "created": "2019-12-26T14:05:02.3412Z",
      "start": 100243,
      "duration": 10000,
      "content": "Agent 1: Victim needs assistance near you",
      "receivers": ["P000132"],
      "type": "string",         
      "renderers": ["Minecraft_Chat"],
      "explanation": "{<agent custom json object>}"       
   }
}
```

```json
{ 
  "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "agent",
    "version": "0.1"
  },
  "msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2019-12-26T14:05:02.1412Z",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "replay_root_id": "123e4567-e89b-12d3-a456-426655440000",
    "source": "tom_generator:1.0",
    "sub_type": "Intervention:Chat",
    "version": "0.5"
  },
  "data": {
      "id": "<uuid for the intervention>",
      "created": "2019-12-26T14:05:02.3412Z",
      "start": -1,
      "duration": 10000,
      "content": "Agent 1: Victim needs assistance near you",
      "receivers": ["P000132"],
      "type": "string",         
      "renderers": ["Minecraft_Chat"],
      "explanation": "{<agent custom json object>}"       
   }
}

```

## Version Change History
VERSION | DATE | DETAILS
| --- | --- | --- | 
| 0.6 | 1/25/2022 | fixed error in documentation where the table showed the source property in the data section when it should be in the msg section
| 0.5 | 3/1/2021 | Ported from Adapt

