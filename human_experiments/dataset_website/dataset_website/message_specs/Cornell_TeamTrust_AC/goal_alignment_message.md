# Cornell TA2  - Goal alignment Event Message Format

This data message subtype, `AC:Goal_alignment`, contains data from the Cornell TA2 Team trust Analytic Component.

## TOPIC
agent/ac/goal_alignment

## Message Fields

| Field Name         | Type          | Description                                  |
| -----------        | ---           | ---------------                              |
| header             | object        | From Common Message Format section           |
| msg                | object        | From Event Header Format section             |
| data.elapsed_ms    | number        | The number of elapsed milliseconds since mission start from the State message |
| data.Red           | object        | Goal alignment measures for Red player       |
| data.Green         | object        | Goal alignment measures for Red player       |
| data.Blue          | object        | Goal alignment measures for Red player       |
| data.Team          | object        | Goal alignment measures for the team         |


## Message Example

```json
{
  "header": {
    "timestamp": "2020-08-25T01:56:21.517415Z",
    "version": "1.1",
    "message_type": "agent"
  },
  "msg": {
    "sub_type": "AC:Goal_alignment",
    "timestamp": "2020-08-22T04:06:18.846Z",
    "version": "0.1",
    "source": "ac_cornell_ta2_teamtrust",
    "experiment_id": "c6f930a2-357b-4c24-9c4e-42b1c8d9458f",
    "trial_id": "25edd30a-2a4b-4229-9237-1304ea8cc439",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75"
  },
  "data": {
    "elapsed_ms": 1111,
    "Red": {
      "current_goal": "Treat",
      "goal_alignment_current": {"Green": "False", "Blue": "True"},
      "goal_alignment_recent": {"Green": 0.5, "Blue": 0.7},
      "goal_alignment_overall": {"Green": 0.3, "Blue": 0.9},
    },
    "Green": {
      "current_goal": "Explore",
      "goal_alignment_current": {"Red": "False", "Blue": "False"},
      "goal_alignment_recent": {"Red": 0.5, "Blue": 0.7},
      "goal_alignment_overall": {"Red": 0.3, "Blue": 0.1},
    },
    "Blue": {
      "current_goal": "Treat",
      "goal_alignment_current": {"Red": "True", "Green": "False"},
      "goal_alignment_recent": {"Red": 0.5, "Green": 0.7},
      "goal_alignment_overall": {"Red": 0.3, "Green": 0.1},
    },
    "Team": {
      "goal_alignment_current": "False",
      "goal_alignment_recent": 0.3,
      "goal_alignment_overall": 0.7,
    }
  }
}
```
