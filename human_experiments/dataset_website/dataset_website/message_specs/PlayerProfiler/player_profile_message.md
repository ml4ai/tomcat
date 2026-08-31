# Player profile Message Format
To be updated


## TOPIC

`agent/PlayerProfiler/playerprofile`

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| player-profile | string | Player Profile category represents the categorization of the player as high or low in task potential and in team potential in order to distinguish players as members of four distinct groups that may display differing tasking and teaming behaviors
| team-potential-category | string | Team potential category represents the categorization of the player as high or low in potential to successfully maintain awareness of their teammates and progress as well as to coordinate activities and resources effectively and efficiently
| task-potential-category | string | Task potential category represents the categorization of the player as high or low in potential to successfully complete mission related actions effectively and efficiently
| callsign | string | The callsign of the player
| participant_id | string | The HSR safe id of the player

Legal values for `player-profile` are `["LowTaskLowTeam", "LowTaskHighTeam", "HighTaskLowTeam", "HighTaskHighTeam"]`

Legal values for `team-potential-category` are `["LowTeam", "HighTeam"]`

Legal values for `task-potential-category` are `["LowTask", "HighTask"]`

For callsign and participant_id, see [client_info.md](../Trial/client_info.md)

## Message Example

```json
{
  "data": {
    "player-profile": "HighTaskLowTeam",
    "team-potential-category": "LowTeam",
    "task-potential-category": "LowTask",
    "participant_id": "P000443",
    "callsign": "Green"
  }
}
```