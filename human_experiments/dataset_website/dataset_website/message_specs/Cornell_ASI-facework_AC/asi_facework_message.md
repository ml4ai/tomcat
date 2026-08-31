# Cornell TA2  - Player compliance Analytic Component Event Message Format

This data message subtype, `AC:Player_compliance`, contains data from the Cornell TA2 Team trust Analytic Component.

## TOPIC
agent/ac/player_compliance

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From Event Header Format section 
| data.elapsed_ms | number | The number of elapsed milliseconds since mission start from the State message |



## Message Example

```json
{
  "header": {
    "timestamp": "2020-08-25T01:56:21.517415Z",
    "version": "1.1",
    "message_type": "agent"
  },
  "msg": {
    "sub_type": "AC:ASI_facework",
    "timestamp": "2020-08-22T04:06:18.846Z",
    "version": "0.1",
    "source": "ac_cornell_ta2_teamtrust",
    "experiment_id": "c6f930a2-357b-4c24-9c4e-42b1c8d9458f",
    "trial_id": "25edd30a-2a4b-4229-9237-1304ea8cc439",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75"
  },
  "data": {
    "elapsed_ms": 70462
  }
}
```
