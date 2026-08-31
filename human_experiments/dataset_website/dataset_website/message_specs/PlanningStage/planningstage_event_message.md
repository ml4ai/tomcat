# Data Subtype: Event Message Format

This data message subtype is used to communicate event data for both the world and the player.

## TOPIC

observations/events/mission/planning

## Message Fields

| Field Name                | Type        | Description                                            |
| ------------------------- | ----------- | ------------------------------------------------------ |
| header                    | object      | From Common Header Format section                      |
| msg                       | object      | From the Common Event Message Format section           |
| data.mission_timer        | string      | the mission time of the event                          |
| data.elapsed_milliseconds | number      | the number of elapsed milliseconds since mission start |
| data.state                | string enum | the new state of the planning stage ['Start', 'Stop ]  |

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
    "sub_type": "Event:PlanningStage",
    "version": "2.0"
  },
  "data": {
    "mission_timer": "8 : 36",
    "elapsed_milliseconds": 15113,
    "state": "Start"
  }
}
```
## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
2.0 | 3/04/2022 | Initial Schema created