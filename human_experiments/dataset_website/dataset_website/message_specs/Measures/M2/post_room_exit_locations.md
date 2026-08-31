# groundtruth subtype : measures Message Format
The measures published at the end of a trial.  

## TOPICS

agent/measures

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| id | string | The id of the location
| time_of_event | number | time in milliseconds 
| player_name | string | the player name 
| time_of_event | string | the room name
| room_name | number | the angle in degrees
| direction | string | the cardinal direction 

## Message Example

```json
{
      "time_of_event" : -1,
      "player_name" : "Player866",
      "room_name" : "Staging Area",
      "angle" : 0.0,
      "direction" : "E"
}
