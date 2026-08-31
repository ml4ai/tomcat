# Data Subtype: Victim Type Communication Event Message Format
This data message subtype is used to measures whether victim type marker blocks are placed near victims of the appropriate types. This is intended to capture whether the team use the marker blocks to effectively communicate about victim types.

agent/ac/victim_type_communication

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format Section
| msg | object | From the Common Event Message Format Section
| data.player_placed | list of string | List of ID of the player who placed the marker block.
| data.time_placed |  list of float | List of time(sec) the marker block is placed in seconds, relative to mission start
| data.marker_block_type | list of string | List of what type of victim marker block is placed.
| data.victim_type_in_vicinity | list of string | List of tuple: what types of victims are within the vicinity_threshold_in_blocks of the marker block (can contain multiple entries.
| data.victims_match_marker_block | list of string | List of tuple of the victim_ids in the vicinity that matches the type of the marker block.
|data.vicinity_threshold_in_blocks | float | Time(sec) that indicated what counts as "vicinity" for the purpose of this AC.## Message Example
|data.time_in_seconds| float | The mission time relative to mission start

```json
{
"header": {"timestamp": "2022-03-14T02:40:28.088641Z", "message_type": "agent", "version": "0.1"}, 
"msg": {"sub_type": "AC:victim_type_communication", "version": 
0.2, "source": "AC_Rutgers_TA2_Utility", "timestamp": "2022-03-14T02:40:28.088641Z", "experiment_id": "934c548a-54ef-4e1e-bdbb-613bd395764b", "trial_id": "add1aeef-a1c0-4621-b7d6-51efe129c99c"}, 
"data": {
    "player_placed": ["RED_ASIST2", "RED_ASIST2", "BLUE_ASIST2", "GREEN_ASIST2", "RED_ASIST2", "RED_ASIST2", "GREEN_ASIST2", "RED_ASIST2", "GREEN_ASIST2", "RED_ASIST2", "RED_ASIST2", "RED_ASIST2", "RED_ASIST2"], 
    "time_placed": [107.141, 182.34, 199.992, 210.591, 342.542, 368.59, 406.491, 422.141, 533.991, 546.84, 598.891, 711.64, 716.341], 
    "marker_block_type": ["A", "B", "A", "B", "B", "A", "B", "B", "B", "B", "B", "B", "A"], 
    "victim_type_in_vicinity": ["A,C", "B", "B", "", "B", "A,B", "", "B", "B", "B", "B", "A,B", "A,B"], 
    "victims_match_marker_block": ["24", "15", "", "", "28", "29", "", "6", "28", "21", "28", "14", "12"],
    "vicinity_threshold_in_blocks": 2, 
    "time_in_seconds": 716.341}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
0.2 | 03/14/2022 | Update key names, description and add time_in_seconds key
0.1 | 03/01/2022 | Add an initial schema definition
