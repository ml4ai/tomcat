# Data Subtype: Event:location Message Format
This data message subtype, `Event:location`, is used to communicate event data for players in the world.

## TOPIC

observations/events/player/location

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the event Header Format section 
| data.participant_id | string | The participant id this data is associated with |
| data.callsign | string | (optional) The callsign of the player                      |
| data.mission_timer | string | the mission time of the event from the State message
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start from the State message
| data.corresponding_observation_number | integer | A sequence number of the number of observations from the beginning of the trial run from the corresponding State message
| data.locations | List of location | (optional) A list of the locations the player is currently in
| data.connections | List of connection | (optional) A list of the connections the player is currently in
| data.exited_locations | List of location | (optional) A list of the locations the player just left
| data.exited_connections | List of connection | (optional) A list of the connections the player just left

## Message Example

```json
{
  "header": {
    "timestamp": "2020-08-25T01:56:21.517415Z",
    "version": "1.1",
    "message_type": "event"
  },
  "msg": {
    "sub_type": "Event:location",
    "timestamp": "2020-08-22T04:06:18.846Z",
    "experiment_id": "c6f930a2-357b-4c24-9c4e-42b1c8d9458f",
    "trial_id": "25edd30a-2a4b-4229-9237-1304ea8cc439",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "version": "3.0",
    "source": "IHMCLocationMonitorAgent"
  },
  "data": {
    "participant_id": "P000999",
    "callsign": "Blue",
    "mission_timer": "8 : 36",
    "elapsed_milliseconds": 15113,
    "corresponding_observation_number": 6857,
    "locations": [
      {
        "id": "lh_2",
        "name": "Part of Left Hallway"
      },
      {
        "id": "lh",
        "name": "Left Hallway"
      }
    ],
    "connections": [
      {
        "id": "ce_112_77_112_79",
        "connected_locations": [
          "lh_1",
          "lh_2"
        ]
      }
    ],
    "exited_locations": [
      {
        "id": "lh_1",
        "name": "Part of Left Hallway"
      }
    ]
  }

}
```