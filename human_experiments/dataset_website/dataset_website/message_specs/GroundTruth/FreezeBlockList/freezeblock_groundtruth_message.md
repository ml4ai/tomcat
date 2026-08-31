# GroundTruth Subtype : Mission:FreezeBlockList Message Format
This GroundTruth message subtype is used to communicate the freeze block list.

## TOPIC

ground_truth/mission/freezeblock_list

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.mission | string | The name of the mission in progress|
| data.mission_freezeblock_list | List of freezeblock_list_item | the list of freezeblocks in the mission 

## Message Example

```json
{
  "header" : {
    "timestamp" : "2021-04-15T17:28:34.055Z",
    "message_type" : "groundtruth",
    "version" : "0.6"
  },
  "msg" : {
    "experiment_id" : "946d4567-ab65-cfe7-b208-426305dc1234",
    "trial_id" : "85ed4567-ab65-cfe7-b208-426305dc1234",
    "timestamp" : "2021-04-15T17:28:34.057Z",
    "source" : "simulator",
    "sub_type" : "Mission:FreezeBlockList",
    "version" : "0.1"
  },
  "data" : {
    "mission" : "Saturn_A",
    "mission_freezeblock_list" : [ {
      "x" : -2199.0,
      "y" : 59.0,
      "z" : 4.0,
      "block_type" : "block_freeze_player",
      "room_name" : "",
      "feature_type" : "threat room"
    }, {
      "x" : -2171.0,
      "y" : 59.0,
      "z" : 9.0,
      "block_type" : "block_freeze_player",
      "room_name" : "",
      "feature_type" : "threat room"
    }, {
      "x" : -2129.0,
      "y" : 59.0,
      "z" : 15.0,
      "block_type" : "block_freeze_player",
      "room_name" : "",
      "feature_type" : "threat room"
    }, {
      "x" : -2104.0,
      "y" : 59.0,
      "z" : 38.0,
      "block_type" : "block_freeze_player",
      "room_name" : "",
      "feature_type" : "threat room"
    }, {
      "x" : -2195.0,
      "y" : 59.0,
      "z" : 40.0,
      "block_type" : "block_freeze_player",
      "room_name" : "",
      "feature_type" : "threat room"
    } ]
  }
}

```
