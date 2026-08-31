# Data Subtype: Event:dyad Message dyad_participant Format
This data message subtype, is used to communicate dyad participant information.

## TOPIC

observations/events/player/dyad

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| callsign | string | The callsign of the player |
| participant_id | string | The participant id this data is associated with |
| role | string | The current role of the player. From ENUM --> [ "None", "Search_Specialist", "Hazardous_Material_Specialist","Medical_Specialist" ] |

## Examples

```json
{ 
  "callsign": "red", 
  "participant_id": "p1", 
  "role": "Search_Specialist" 
}
```
