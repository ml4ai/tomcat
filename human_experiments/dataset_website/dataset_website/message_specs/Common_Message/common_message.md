# Common Data Message Header Format
A data message is used to communicate information from one component of the testbed to another component.  
## Message Fields

| Field Name | Type | Description
| --- | --- | --- |
| msg.experiment_id | string | The experiment id this message is associated with
| msg.trial_id | string | The trial id this message is associated with
| msg.timestamp | string | Timestamp of when the data was generated in ISO 8601 format: YYYY-MM-DDThh:mm:ss.ssssZ
| msg.source | string | The name of the testbed component that published this data
| msg.sub_type | string | The subtype of the data.  This field describes the format of this particular type of data
| msg.version | string | The version of the sub_type format
| msg.replay_parent_type | string | The parent type. From ENUM --> [ "TRIAL", "REPLAY" ].
| msg.replay_parent_id | string | The replay_parent_id if being used for the parent of a replay.
| msg.replay_id | string | The replay_id if being used for a replay.

## Message Example

### This object should be included in all event messages along with the "header" (Header_Commmon/common_header.md) and "data" (unique to each message) objects. It should always have the key "msg".

## TOPIC

observations/state


```json

"msg": {
  "experiment_id":"123e4567-e89b-12d3-a456-426655440000",
  "trial_id": "123e4567-e89b-12d3-a456-426655440000",
  "timestamp": "2019-12-26T14:05:02.1412Z",
  "source": "simulator",
  "sub_type": "pickup",
  "version": "0.6",
  "replay_parent_type": "TRIAL",
  "replay_parent_id": null,  
  "replay_id": "876e4567-ab65-cfe7-b208-426305dc1234",
}

```
## Version Change History
VERSION | DATE | DETAILS
| --- | --- | --- | 
| 0.6 | 4/12/2021 | Added "replay_parent_type", "replay_parent_id" | 
| 0.5 | 3/1/2021 | Version tracking begins 