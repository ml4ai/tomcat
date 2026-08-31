# Data Subtype: Event Message Format

This data message subtype is used to communicate event data for both the world and the player.

## TOPIC

observations/events/mission/perturbation

## Message Fields

| Field Name                | Type        | Description                                            |
| ------------------------- | ----------- | ------------------------------------------------------ |
| header                    | object      | From Common Header Format section                      |
| msg                       | object      | From the Common Event Message Format section           |
| data.mission_timer        | string      | the mission time of the event                          |
| data.elapsed_milliseconds | number      | the number of elapsed milliseconds since mission start |
| data.type                 | string enum | The name of the perturbation type ['blackout','rubble']         |
| data.mission_state                | string enum | the new state of the perturbation ['start','stop]      |

## Message Example

```json
{
  "header": {
    "timestamp": "2019-12-26T12:47:23.1234Z",
    "message_type": "event",
    "version": "0.5"
  },
  "msg": {
    "experiment_id": "563e4567-e89b-12d3-a456-426655440000",
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2019-12-26T14:05:02.1412Z",
    "source": "simulator",
    "sub_type": "Event:Perturbation",
    "version": "2.0"
  },
  "data": {
    "mission_timer": "8 : 36",
    "elapsed_milliseconds": 15113,
    "type": "blackout",
    "mission_state": "start"
  }
}
```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
2.2 | 1/24/2021 | Updated the Message Fields section "state" key to "mission_state" in this file. Also made mission_state a required key as opposed to state
2.1 | 12/23/2021 | Added enum type ['rubble'] to type field
2.0 | 11/30/2021 | Initial spec created
