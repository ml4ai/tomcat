# Data Subtype: Belief Difference Event Message Format
This data message subtype computes the beliefs about victim distribution under three different assumptions: (1) players share information perfectly, (2) players don't share any information, and (3) players only share information via marker blocks.

## TOPIC

agent/ac/belief_diff

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format Section
| msg | object | From the Common Event Message Format Section
| data.room_id | list of string | List of all room ids in sequence, ending with an "overall" entry, for the summed value across all rooms
| data.shared | list of float | List of entropy on victim type and number for each room, and summed across rooms (for the full map overview)
| data.RED_indiv |  list of float | List of entropy on victim type and number based on direct information for the medic
| data.BLUE_indiv | list of float | List of entropy on victim type and number based on direct information for the engineer
| data.GREEN_indiv | list of float | List of entropy on victim type and number based on direct information for the transporter
| data.RED_marker |  list of float | List of entropy on victim type and number based on direct information for the medic and marker block information
| data.BLUE_marker | list of float | List of entropy on victim type and number based on direct information for the engineer and marker block information
| data.GREEN_marker | list of float | List of entropy on victim type and number based on direct information for the transporter and marker block information
|data.time_in_seconds| float | The mission time(sec) relative to mission start
## Message Example

```json
{
"header": {"timestamp": "2022-03-14T02:39:45.853896Z", "message_type": "agent", "version": "0.1"},
"msg": {"sub_type": "AC:belief_diff", "version": 0.1, "source": "AC_Rutgers_TA2_Utility", "timestamp": "2022-03-14T02:39:45.853896Z", "experiment_id": "934c548a-54ef-4e1e-bdbb-613bd395764b", "trial_id": "add1aeef-a1c0-4621-b7d6-51efe129c99c"},
"data": {
    "room_id": ["A1", "A2", "A3", "A4", "A4A", "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "D1", "D2", "D3", "D4", "E1", "E2", "E3", "E4", "E5", "F1", "F2", "F3", "F4", "G1", "G2", "G3", "H1", "H1A", "H2", "I1", "I2", "I3", "I4", "I1A", "I2A", "I3A", "I4A", "J1", "J2", "J3", "J4", "K1", "K2", "K3", "K4", "L1", "L2", "L3", "M1", "M2", "M3", "overall"],
    "shared": [1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 0.0, 0.0, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 0.0, 1.116, 0.0, 0.0, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.116, 1.116, 1.116, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 36.839],
    "RED_indiv": [0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.0, 0.0, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.0, 0.998, 0.0, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.998, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.998, 0.998, 0.0, 0.998, 0.998, 0.998, 0.0, 0.998, 0.998, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.998, 0.0, 38.936],
    "BLUE_indiv": [1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 0.0, 0.0, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 0.0, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 1.049, 0.0, 0.0, 0.0, 0.0, 0.0, 1.049, 1.049, 0.0, 1.049, 0.0, 1.049, 1.049, 1.049, 0.0, 1.049, 0.0, 1.049, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.049, 1.049, 0.0, 0.0, 0.0, 44.078],
    "GREEN_indiv": [1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 0.0, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 0.0, 0.0, 0.0, 0.0, 1.249, 1.249, 0.0, 0.0, 0.0, 0.0, 1.249, 1.249, 1.249, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 48.717],
    "RED_marker": [1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 0.0, 0.0, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 0.0, 1.116, 0.0, 0.0, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 1.116, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.116, 1.116, 1.116, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 36.839],
    "BLUE_marker": [1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 0.0, 0.0, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 0.0, 0.0, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 1.18, 0.0, 0.0, 0.0, 0.0, 0.0, 1.18, 0.0, 0.0, 0.0, 0.0, 1.18, 1.18, 1.18, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 41.328],
    "GREEN_marker": [1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 0.653, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 0.0, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 1.249, 0.0, 0.0, 0.0, 0.0, 0.653, 1.249, 0.0, 0.0, 0.0, 0.0, 1.249, 1.249, 1.249, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 47.525],
    "time_in_seconds": 609.317}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |

0.1 | 03/14/2022 | Add an initial schema definition
