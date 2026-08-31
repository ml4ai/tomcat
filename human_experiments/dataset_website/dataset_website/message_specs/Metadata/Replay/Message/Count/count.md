# Replay Count Format
A replay count message describes the current and total message counts for replay status tracking.  

## TOPICS

trial

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| replay_id | string | The uuid string for the replay.
| current_message_count | integer | The current message count of the running replay.
| total_message_count | integer | The total message count of the running replay.

## Message Examples
```json
{
  "replay_id": 123e4567-e89b-12d3-a456-426655449999,
  "current_message_count": 0,
  "total_message_count": 100
}
```