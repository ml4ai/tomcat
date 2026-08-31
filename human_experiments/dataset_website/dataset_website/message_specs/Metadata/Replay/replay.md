# Replay Message Format
A replay message describes the characteristics of a replay.  

## TOPICS

trial

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| id | number | The database id of the replay.
| replay_id | string | The uuid string for the replay.
| replay_parent_id | string | The uuid string for the parent replay.
| replay_parent_type | string | The parent type. From ENUM --> [ "TRIAL", "REPLAY" ].
| date | string | The date and time the replay was run.
| ignore_list | string array | A lists of types to igonore when exporting.

## Message Examples
```json
{
  "id": "1",
  "replay_id": 123e4567-e89b-12d3-a456-426655449999,
  "replay_parent_id": 123e4567-e89b-12d3-a456-426655440000,
  "replay_parent_type": "TRIAL",
  "date": "2019-12-26T14:05:02.3412Z",
  "ignore_list": [
    "message_type 1",
    "message_type 2",
    "message_type 3"
  ]
}
```