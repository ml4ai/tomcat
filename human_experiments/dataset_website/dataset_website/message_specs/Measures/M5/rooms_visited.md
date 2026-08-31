# groundtruth subtype : measures Message Format
The measures published at the end of a trial.  

## TOPICS

agent/measures

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| room_id | string | The room id
| room_name | string | The room name
| Medical_Specialist | number | The count of medical visits
| Search_Specialist | number | The count of search visits
| Hazardous_Material_Specialist | number | The count of hazard visits
| visitors | array | The list of all players visited

## Message Example

```json
{
    "room_id" : "sga",
    "room_name" : "Staging Area",
    "Medical_Specialist" : 0,
    "Search_Specialist" : 1,
    "Hazardous_Material_Specialist" : 0,
    "visitors" : [ {
        "player" : {
        "name" : "Player866",
        "role" : "Search_Specialist"
        },
        "time_elapsed_milliseconds" : -1
    } ]
}