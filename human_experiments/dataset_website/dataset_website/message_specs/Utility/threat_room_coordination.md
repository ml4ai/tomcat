# Data Subtype: Threat Room Coordination Event Message Format
This data message subtype is used to measure time from threat plate activation until the associated threat rubble is removed, and which player activates the threat plate. This measure is intended to capture how efficiently the team coordinates to rescue team mates stuck in threat rooms.
## TOPIC

agent/ac/threat_room_coordination

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format Section
| msg | object | From the Common Event Message Format Section
| data.room_id | list of string | List of room ids of activated threat plate
| data.threat_activation_time |  list of float | List of threat plate activation time in seconds from mission start
| data.threat_activation_player | list of string | List of player id of the player who activated the threat plate
| data.wait_time | list of float | List of difference in seconds from threat_activation_time until the threat rubble is removed
|data.threshold_time| float | Single entry giving the threshold for wait time for the AC to flag that a participant appears to be stuck. The threshold is set to 10 seconds, which corresponds to five seconds less than the median wait time in the pilot data.
|data.time_in_seconds| float | The mission time relative to mission start

## Message Example

```json
{
"header": {"timestamp": "2022-03-14T02:39:21.862254Z", "message_type": "agent", "version": "0.1"}, 
"msg": {"sub_type": "AC:threat_room_coordination", "version": 0.2, "source": "AC_Rutgers_TA2_Utility", "timestamp": "2022-03-14T02:39:21.862254Z", "experiment_id": "934c548a-54ef-4e1e-bdbb-613bd395764b", "trial_id": "add1aeef-a1c0-4621-b7d6-51efe129c99c"}, 
"data": {
    "room_id": ["H1A", "J4", "H1A", "J4", "K2", "M3", "D3"], 
    "threat_activation_time": [80.745, 87.143, 120.742, 173.342, 310.041, 408.342, 539.691], 
    "threat_activation_player": ["RED_ASIST2", "BLUE_ASIST2", "RED_ASIST2", "RED_ASIST2", "RED_ASIST2", "RED_ASIST2", "RED_ASIST2"], 
    "wait_time": [Infinity, 20.947000000000003, 14.897999999999982, 1.899000000000001, 29.25, 2.4530000000000314, Infinity], 
    "threshold:": 10,
    "time_in_seconds": 552.521}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
0.2 | 03/14/2022 | Update key names, description and add time_in_seconds key
0.1 | 03/01/2022 | Add an initial schema definition
