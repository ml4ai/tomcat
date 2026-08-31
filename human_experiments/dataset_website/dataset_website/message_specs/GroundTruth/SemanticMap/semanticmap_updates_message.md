# GroundTruth Subtype:SemanticMap:Update Message Format
This GroundTruth message subtype is used to communicate the updates to the semantic map.

## TOPIC

ground_truth/semantic_map/updates

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.semantic_map_name | string | (optional) The name of the semantic map which these updates were applied to.  Valid only when the msg sub_type is 'SemanticMap:All_Updates'.|
| data.updates | object | the list of which have been applied to the semantic map

## Message Example

```json
{
  "header": {
    "timestamp": "2020-08-25T01:56:20.943284Z",
    "version": "1.0",
    "message_type": "groundtruth"
  },
  "msg": {
    "sub_type": "SemanticMap:All_Updates",
    "timestamp": "2020-08-22T04:06:08.253Z",
    "experiment_id": "c6f930a2-357b-4c24-9c4e-42b1c8d9458f",
    "trial_id": "25edd30a-2a4b-4229-9237-1304ea8cc439",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "version": "2.0",
    "source": "IHMCLocationMonitorAgent"
  },
  "data": {
    "semantic_map_name": "Falcon_sm_v1.0.json",
    "updates": {
      "modifications": {
        "locations": [
          {
            "id": "lh_2",
            "bounds": null,
            "child_locations": [ "lh_2_1", "lh_2_2" ]
          }
        ],
        "connections": [
          {
            "id": "c_158_76_160_77",
            "connected_locations": [ "kco_4", "lh_2_2" ]
          }
        ]
      },
      "additions": {
        "locations": [
          {
            "id": "lh_2_1",
            "name": "Part of lh_2",
            "type": "hallway_part",
            "bounds": {
              "type": "rectangle",
              "coordinates": [ { "x": -2089, "z": 152 }, { "x": -2048, "z": 156 } ]
            }
          },
          {
            "id": "lh_2_2",
            "name": "Part of lh_2",
            "type": "hallway_part",
            "bounds": {
              "type": "rectangle",
              "coordinates": [ { "x": -2047, "z": 152 }, { "x": -2037, "z": 156  } ]
            }
          }
        ],
        "connections": [
          {
            "id": "c_128_115_129_116",
            "type": "opening",
            "bounds": {
              "type": "rectangle",
              "coordinates": [ { "x": -2073, "z": 190 }, { "x": -2072, "z": 191 } ]
            },
            "connected_locations": [
              "r108_2",
              "r107_1"
            ]
          }
        ],
        "objects": [
          {
            "id": "vg1",
            "type": "green_victim",
            "bounds": {
              "type": "block",
              "coordinates": [ { "x": -2077, "z": 145 } ]
            }
          }
        ]
      }
    }
  }
}
```