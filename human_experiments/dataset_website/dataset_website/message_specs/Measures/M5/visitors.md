# groundtruth subtype : measures Message Format
The measures published at the end of a trial.  

## TOPICS

agent/measures

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| id | string | The id of the location
| player | object | the player 
| player.name | string | the player name 
| player.role | string | the map name 
| time_elapsed_milliseconds | number | the player name 


## Message Example

```json
{
    "player" : {
      "name" : "Player866",
      "role" : "Search_Specialist"
    },
    "time_elapsed_milliseconds" : -1
}