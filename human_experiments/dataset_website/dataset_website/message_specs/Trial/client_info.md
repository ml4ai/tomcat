# Trial Message Format
A trial message describes the characteristics of the client_info portion of a trial.  

## TOPICS

trial
## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| id | string | The id of the location
| playername | string | (optional) The name of the player 
| callsign | string | (optional) The callsign of the player 
| participant_id | string | (optional) The participantid of the player 
| staticmapversion | string | (optional) The staticmapversion of the player 
| markerblocklegend | string | (optional) The markerblocklegend of the player 
| unique_id | string | (optional) The unique id of the player 


## TOPIC
trial

## Message Example

```json
{
      "playername" : "testplayer",
      "callsign" : "Bravo",
      "participant_id" : "foo",
      "staticmapversion" : "SaturnA_64",
      "markerblocklegend" : "B_Sally",
      "unique_id" : "bar"

}

```