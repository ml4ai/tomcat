# groundtruth subtype : measures Message Format
The measures published at the end of a trial.  

## TOPICS

agent/measures

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| victim_location | object | The position of the critical victim
| loiter_time | number | the time between the first and last player arriving at the victim 
| player_arrivals | array | the names and times players arrive at critical victim 

## Message Example

```json
{
    "victim_location" : {
        "x" : -2169,
        "z" : 29
    },
    "loiter_time" : 0.0,
    "player_arrivals" : [ {
        "player_name" : "Player866",
        "arrival_time" : 21002
    } ]
}