# Data Subtype: Event:proximity Message Format
This data message subtype, `Event:proximity`, is used to communicate player proximity (travel distance) to other players
and locations in the world.

## TOPIC

observations/events/player/proximity

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the event Header Format section 
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start |
| data.participants | List of Proximity | list of proximity objects for each participant |

## Message Example

```json
{
    "header": {
        "timestamp": "2021-10-11T01:05:29.826595Z",
        "message_type": "event",
        "version": "1.1"
    },
    "msg": {
        "sub_type": "Event:proximity",
        "version": "1.0",
        "source": "IHMC_Proximity_AC",
        "timestamp": "2021-10-11T01:05:29.826595Z",
        "experiment_id": "39decb46-72fc-4a54-9a78-7571b6011e27",
        "trial_id": "3c6adbb6-ee33-4e7e-8d00-296d27a0d32a"
    },
    "data": {
        "elapsed_milliseconds": 51579,
        "participants": [
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
            },
            {
                "callsign": "green",
                "participant_id": "p2",
                "role": "None",
                "current_location": "sga",
                "distance_to_participants": [
                    { "id": "red", "distance": 132.9 },
                    { "id": "blue", "distance": 0.0 }
                ],
                "distance_to_current_location_exits": [
                    { "id": "c_46_35_49_36", "distance": 15.0 }
                ],
                "distance_to_closest_locations": [
                    { "id": "ca", "distance": 73.7 },
                    { "id": "br", "distance": 84.7 },
                    { "id": "o100", "distance": 83.7 },
                    { "id": "o101", "distance": 78.7 },
                    { "id": "sdc", "distance": 83.0 }
                ],
                "distance_to_role_change": [
                    { "id": "sga", "distance": 0.0 }
                ],
                "distance_to_treatment_areas": []
            },
            {
                "callsign": "blue",
                "participant_id": "p3",
                "role": "None",
                "current_location": "sga",
                "distance_to_participants": [
                    { "id": "red", "distance": 132.9 },
                    { "id": "green", "distance": 0.0 }
                ],
                "distance_to_current_location_exits": [
                    { "id": "c_46_35_49_36", "distance": 15.0 }
                ],
                "distance_to_closest_locations": [
                    { "id": "ca", "distance": 73.7 },
                    { "id": "br", "distance": 84.7 },
                    { "id": "o100", "distance": 83.7 },
                    { "id": "o101", "distance": 78.7 },
                    { "id": "sdc", "distance": 83.0 }
                ],
                "distance_to_role_change": [
                    { "id": "sga", "distance": 0.0 }
                ],
                "distance_to_treatment_areas": []
            }
        ]
    }
}
```