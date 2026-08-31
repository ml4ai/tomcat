# GroundTruth Subtype:SemanticMap:Initialized Message Format
This GroundTruth message subtype is used to communicate the Initial state of the semantic map.

## TOPIC

ground_truth/semantic_map/initialized

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section
| msg | object | From the Common Event Message Format section 
| data.semantic_map_name | string | The name of the semantic map.|
| data.semantic_map | object | the JSON formatted Semantic map

## Message Example

```json
{
  "header": {
    "timestamp": "2020-08-25T01:56:20.943284Z",
    "version": "1.0",
    "message_type": "groundtruth"
  },
  "msg": {
    "sub_type": "SemanticMap:Initialized",
    "timestamp": "2020-08-22T04:06:08.253Z",
    "experiment_id": "c6f930a2-357b-4c24-9c4e-42b1c8d9458f",
    "trial_id": "25edd30a-2a4b-4229-9237-1304ea8cc439",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "version": "2.0",
    "source": "IHMCLocationMonitorAgent"
  },
  "data": {
    "semantic_map_name": "Falcon_sm_v1.0.json",
    "semantic_map": {
      "locations": [
        {
          "id": "lh",
          "name": "Left Hallway",
          "type": "hallway",
          "bounds": {
            "type": "rectangle",
            "coordinates": [ { "x": -2089, "z": 152 }, { "x": -2048, "z": 156 } ]
          }
        },
        {
          "id": "rh",
          "name": "Right Hallway",
          "type": "hallway",
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
            "coordinates": [ { "x": -2048, "z": 152 }, { "x": -2047, "z": 156 } ]
          },
          "connected_locations": [
            "lh",
            "rh"
          ]
        }
      ],
      "objects": [
        {
          "id": "vg1",
          "type": "green_victim",
          "bounds": {
            "type": "block",
            "coordinates": [ { "x": -2053, "z": 154 } ]
          }
        }
      ]
    }
  }
}
```