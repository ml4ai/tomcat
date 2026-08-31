# Data Subtype: Event:proximity Message proximity Format
This data message subtype, is used to communicate the proximity info for individual players.

## TOPIC

observations/events/player/proximity

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| callsign | string | The callsign of the player |
| participant_id | string | The participant id this data is associated with |
| role | string | The current role of the player. From ENUM --> [ "None", "Search_Specialist", "Hazardous_Material_Specialist","Medical_Specialist" ] |
| current_location | string | The id of the location the player is currently in from the semantic map definition |
| distance_to_participants | List of Distance | List of distances to other players the 'id' value is the callsign of the other player |
| distance_to_current_location_exits | List of Distance | List of distances to the exits for the current location.  Hallways do not report exits and the 'id' value is a connection id from the semantic map definition |
| distance_to_closest_locations | List of Distance | List of distances to locations within a specified distance (default is 100 blocks).  The 'id' value is a location id from the semantic map definition. |
| distance_to_role_change | List of Distance | List of distances to role change locations.  The 'id' value is a location id from the semantic map definition. |
| distance_to_treatment_areas | List of Distance | List of distances to treatment areas.  The 'id' value is a location id from the semantic map definition. |

## Example

```json
{
    "callsign": "red",
    "participant_id": "p1",
    "role": "Search_Specialist",
    "current_location": "sre",
    "distance_to_participants": [ 
        { "id": "green", "distance": 132.9 },
        { "id": "blue", "distance": 132.9 }
    ],
    "distance_to_current_location_exits": [
        { "id": "c_78_-46_79_-45", "distance": 4.6 },
        { "id": "c_72_-48_73_-47", "distance": 2.0 }
    ],
    "distance_to_closest_locations": [
        { "id": "r103", "distance": 82.7 },
        { "id": "r110", "distance": 97.2 },
            ...
        { "id": "srh", "distance": 23.8 },
        { "id": "srf", "distance": 2.0 },
        { "id": "srq", "distance": 53.7 }
    ],
    "distance_to_role_change": [
        { "id": "sga", "distance": 132.9 }
    ],
    "distance_to_treatment_areas": []
}
```