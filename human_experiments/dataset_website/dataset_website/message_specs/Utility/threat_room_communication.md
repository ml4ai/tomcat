# Data Subtype: Threat Room Communication Event Message Format
This data message subtype is used to measures whether a threat room marker block is placed outside of a threat room.
## TOPIC

agent/ac/threat_room_communication

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format Section
| msg | object | From the Common Event Message Format Section
| data.player_placed | list of string | List of id of the player who placed the marker block
| data.time_placed | list of float | List of time the marker block is placed in seconds, relative to mission start
| data.nearest_room |  list of string | List of id of the room closest to the threat room marker block
| data.is_observed_threat_room | list of boolean | List of boolean indicating whether this room is a known threat room
| data.time_in_seconds | float | The mission time relative to mission start

## Message Example

```json
{
"header": {"timestamp": "2022-03-14T02:40:26.567636Z", "message_type": "agent", "version": "0.1"},
"msg": {"sub_type": "AC:threat_room_communication", "version":
0.2, "source": "AC_Rutgers_TA2_Utility", "timestamp": "2022-03-14T02:40:26.567636Z", "experiment_id": "934c548a-54ef-4e1e-bdbb-613bd395764b", "trial_id": "add1aeef-a1c0-4621-b7d6-51efe129c99c"},
"data": {
    "player_placed": ["BLUE_ASIST2", "RED_ASIST2", "RED_ASIST2", "BLUE_ASIST2", "BLUE_ASIST2", "BLUE_ASIST2"],
    "time_placed": [110.591, 236.991, 327.89, 342.545, 343.341, 713.341],
    "nearest_room": ["J4", "K2", "K2", "K2", "K2", "A4"],
    "is_observed_threat_room": [true, false, true, true, true, false],
    "time_in_seconds:"609.317}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
0.2 | 03/14/2022 | Update key names, description and add time_in_seconds key
0.1 | 03/10/2022 | Add an initial schema definition
