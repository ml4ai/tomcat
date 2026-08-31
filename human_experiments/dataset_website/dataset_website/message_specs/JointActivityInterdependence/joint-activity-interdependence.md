# IHMC TA2 - Joint Activity Interdependence Event Message Format

## TOPIC

`observations/events/player/jag`

## Discovery

### Event

* `Event:Discovered`

Published when a new joint activity instance is identified by a given participant

#### Message Fields

| Field Name                  | Type   | Description                                                                      | Examples                                 |
|-----------------------------|--------|----------------------------------------------------------------------------------|------------------------------------------|
| header                      | object | From Common Message Format section                                               |                                          |
| msg                         | object | From Event Header Format section                                                 |                                          |
| data.participant_id         | string | Participant id                                                                   | `"P000123"`                              |
| data.jag                    | object | Root node of joint activity instance                                             |                                          |
| data.jag.id                 | string | Joint activity instance identifier (uuidv4) used as reference for status updates | `"73fc0e47-b09e-47de-badd-8ca3e42e6ee2"` |
| data.jag.urn                | string | Joint activity identifier                                                        | `"urn:ihmc:asist:rescue-victim"`         |
| data.jag.inputs             | object | Mapping of input names to input values                                           | `{"victim-id": 23}`                      |
| data.jag.outputs            | object | Mapping of output names to output values                                         | `{"victim-type": "victim-a"}`            |
| data.jag.children           | array  | child instances of this jag instance                                             |                                          |

#### Message Example

```json
{
  "header": {
    "timestamp": "2022-04-05T23:05:11.243061Z",
    "message_type": "event",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "Event:Discovered",
    "version": "1.2.8",
    "source": "ac_ihmc_ta2_joint-activity-interdependence",
    "timestamp": "2022-04-05T23:05:11.243061Z",
    "experiment_id": "9d6fa759-91de-469b-98ed-672353554fa1",
    "trial_id": "74a9cadc-f892-4be1-a241-b6a645ba477b"
  },
  "data": {
    "participant_id": "P000463",
    "jag": {
      "id": "73fc0e47-b09e-47de-badd-8ca3e42e6ee2",
      "urn": "urn:ihmc:asist:rescue-victim",
      "inputs": {
        "victim-id": 19,
        "victim-type": "critical"
      },
      "outputs": {},
      "children": [
        {
          "id": "a462efd8-adea-4fd3-af6d-3ae8d71abb98",
          "urn": "urn:ihmc:asist:access-victim",
          "inputs": {
            "victim-id": 19,
            "victim-type": "critical"
          },
          "outputs": {},
          "children": [
            {
              "id": "bed4e819-4f19-4d7b-8848-360e418b16e6",
              "urn": "urn:ihmc:asist:check-if-unlocked",
              "inputs": {
                "victim-id": 19,
                "victim-type": "critical"
              },
              "outputs": {},
              "children": []
            },
            {
              "id": "12185fdc-cd5b-4a1b-92f7-cee591beaf6c",
              "urn": "urn:ihmc:asist:unlock-victim",
              "inputs": {
                "victim-id": 19,
                "victim-type": "critical"
              },
              "outputs": {},
              "children": []
            }
          ]
        },
        {
          "id": "b22b0d11-808c-4dde-b074-077a81e804f8",
          "urn": "urn:ihmc:asist:triage-and-evacuate",
          "inputs": {
            "victim-id": 19,
            "victim-type": "critical"
          },
          "outputs": {},
          "children": [
            {
              "id": "18b96df7-e281-45db-9c70-de7ce6b2eea4",
              "urn": "urn:ihmc:asist:triage-victim",
              "inputs": {
                "victim-id": 19,
                "victim-type": "critical"
              },
              "outputs": {},
              "children": [
                {
                  "id": "e60ab2c0-abcf-41da-af05-e5b483fbcce1",
                  "urn": "urn:ihmc:asist:stabilize",
                  "inputs": {
                    "victim-id": 19,
                    "victim-type": "critical"
                  },
                  "outputs": {},
                  "children": []
                }
              ]
            },
            {
              "id": "7535166f-71fa-4e8a-a045-ba2d05ff0034",
              "urn": "urn:ihmc:asist:evacuate-victim",
              "inputs": {
                "victim-id": 19,
                "victim-type": "critical"
              },
              "outputs": {},
              "children": [
                {
                  "id": "5d1d3b8b-c43d-4d3c-a030-559a070e04c7",
                  "urn": "urn:ihmc:asist:determine-triage-area",
                  "inputs": {
                    "victim-id": 19,
                    "victim-type": "critical"
                  },
                  "outputs": {},
                  "children": [
                    {
                      "id": "2a433488-df97-4ea3-9b71-a1eec29d814c",
                      "urn": "urn:ihmc:asist:diagnose",
                      "inputs": {
                        "victim-id": 19,
                        "victim-type": "critical"
                      },
                      "outputs": {},
                      "children": []
                    }
                  ]
                },
                {
                  "id": "641a3397-a7b6-4b74-a620-2f58400e5560",
                  "urn": "urn:ihmc:asist:move-victim-to-triage-area",
                  "inputs": {
                    "victim-id": 19,
                    "victim-type": "critical"
                  },
                  "outputs": {},
                  "children": [
                    {
                      "id": "4339fd6d-a32f-4343-8aa3-1fd6b0153bcd",
                      "urn": "urn:ihmc:asist:relocate-victim",
                      "inputs": {
                        "victim-id": 19,
                        "victim-type": "critical"
                      },
                      "outputs": {},
                      "children": [
                        {
                          "id": "7168f432-2a41-4956-8d60-37b82df7954d",
                          "urn": "urn:ihmc:asist:pick-up-victim",
                          "inputs": {
                            "victim-id": 19,
                            "victim-type": "critical"
                          },
                          "outputs": {},
                          "children": []
                        },
                        {
                          "id": "b2309e21-592f-43b9-a3aa-1b5edccf1c92",
                          "urn": "urn:ihmc:asist:drop-off-victim",
                          "inputs": {
                            "victim-id": 19,
                            "victim-type": "critical"
                          },
                          "outputs": {},
                          "children": []
                        }
                      ]
                    },
                    {
                      "id": "802b46e2-0bd6-4eaf-8f8b-55d3650d377e",
                      "urn": "urn:ihmc:asist:at-proper-triage-area",
                      "inputs": {
                        "victim-id": 19,
                        "victim-type": "critical"
                      },
                      "outputs": {},
                      "children": []
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }
  }
}
```

### Event

* `Event:Awareness`, `Event:Addressing`, `Event:Completion`

Published on joint activity instance status update

#### Message Fields

| Field Name                   | Type    | Description                                                                                              | Examples                                 |
|------------------------------|---------|----------------------------------------------------------------------------------------------------------|------------------------------------------|
| header                       | object  | From Common Message Format section                                                                       |                                          |
| msg                          | object  | From Event Header Format section                                                                         |                                          |
| data.participant_id          | string  | Participant id                                                                                           | `"P000123"`                              |
| data.jag                     | object  | Root node of joint activity instance                                                                     |                                          |
| data.jag.id                  | string  | Joint activity instance identifier (uuidv4)                                                              | `"e60ab2c0-abcf-41da-af05-e5b483fbcce1"` |
| data.jag.elapsed_millisecond | number  | Time at which the update happens (matches the time that triggered the event responsible for that update) | `42329`                                  |
| data.jag.is_complete         | boolean | Indicates this jag node completion status                                                                | `true`, `false`                          |
| data.jag.awareness           | object  | Mapping of players to awareness values from the perspective of `data.participand_id`                     | `{"P000433": 1.0}`                       |
| data.jag.addressing          | object  | Mapping of players to addressing values from the perspective of `data.participand_id`                    | `{"P000433": 0.0}`                       |
| data.jag.preparing           | object  | Mapping of players to preparing values from the perspective of `data.participand_id`                     | `{"P000433": 0.0}`                       |

## Message Example

### Awareness

```json
{
  "header": {
    "timestamp": "2022-04-05T23:05:11.249240Z",
    "message_type": "event",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "Event:Awareness",
    "version": "1.2.8",
    "source": "ac_ihmc_ta2_joint-activity-interdependence",
    "timestamp": "2022-04-05T23:05:11.249240Z",
    "experiment_id": "9d6fa759-91de-469b-98ed-672353554fa1",
    "trial_id": "74a9cadc-f892-4be1-a241-b6a645ba477b"
  },
  "data": {
    "participant_id": "P000463",
    "jag": {
      "id": "e60ab2c0-abcf-41da-af05-e5b483fbcce1",
      "elapsed_milliseconds": 180343,
      "urn": "urn:ihmc:asist:stabilize",
      "is_complete": false,
      "awareness": {
        "P000463": 1.0
      }
    }
  }
}
```

### Preparing

```json
{
  "header": {
    "timestamp": "2022-04-05T23:05:14.658147Z",
    "message_type": "event",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "Event:Preparing",
    "version": "1.2.8",
    "source": "ac_ihmc_ta2_joint-activity-interdependence",
    "timestamp": "2022-04-05T23:05:14.658147Z",
    "experiment_id": "9d6fa759-91de-469b-98ed-672353554fa1",
    "trial_id": "74a9cadc-f892-4be1-a241-b6a645ba477b"
  },
  "data": {
    "participant_id": "P000463",
    "jag": {
      "id": "e60ab2c0-abcf-41da-af05-e5b483fbcce1",
      "elapsed_milliseconds": 181741,
      "is_complete": false,
      "preparing": {
        "P000463": 1.0
      }
    }
  }
}
```

### Addressing

```json
{
  "header": {
    "timestamp": "2022-04-05T23:05:14.657273Z",
    "message_type": "event",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "Event:Addressing",
    "version": "1.2.8",
    "source": "ac_ihmc_ta2_joint-activity-interdependence",
    "timestamp": "2022-04-05T23:05:14.657273Z",
    "experiment_id": "9d6fa759-91de-469b-98ed-672353554fa1",
    "trial_id": "74a9cadc-f892-4be1-a241-b6a645ba477b"
  },
  "data": {
    "participant_id": "P000463",
    "jag": {
      "id": "e60ab2c0-abcf-41da-af05-e5b483fbcce1",
      "elapsed_milliseconds": 183735,
      "is_complete": false,
      "addressing": {
        "P000463": 1.0
      }
    }
  }
}
```

### Completion

```json
{
  "header": {
    "timestamp": "2022-04-05T23:05:17.367529Z",
    "message_type": "event",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "Event:Completion",
    "version": "1.2.8",
    "source": "ac_ihmc_ta2_joint-activity-interdependence",
    "timestamp": "2022-04-05T23:05:17.367529Z",
    "experiment_id": "9d6fa759-91de-469b-98ed-672353554fa1",
    "trial_id": "74a9cadc-f892-4be1-a241-b6a645ba477b"
  },
  "data": {
    "participant_id": "P000463",
    "jag": {
      "id": "e60ab2c0-abcf-41da-af05-e5b483fbcce1",
      "elapsed_milliseconds": 186481,
      "is_complete": true,
      "addressing": {
        "P000463": 0.0
      }
    }
  }
}
```
### Event

* `Event:Summary`

Summary of joint activity efficiency published on joint activity instance completion

#### Message Fields

| Field Name                         | Type   | Description                                                                                    | Examples                                                                                                 |
|------------------------------------|--------|------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------|
| header                             | object | From Common Message Format section                                                             |                                                                                                          |
| msg                                | object | From Event Header Format section                                                               |                                                                                                          |
| data.jag                           | object | Joint activity for which the summary information is computed                                   |                                                                                                          |
| data.jag.instances                 | object | Mapping of player to joint activity instance identifiers as published in the discovery message | `{"P000463": "73fc0e47-b09e-47de-badd-8ca3e42e6ee2", "P000464": "dd28a91d-d3d0-4b51-a21f-f1ba624e8262"}` |
| data.jag.urn                       | string | Joint activity identifier - for human consumption                                              | `"urn:ihmc:asist:rescue-victim"`                                                                         |
| data.jag.inputs                    | object | Mapping of input names to input values - for human consumption                                 | `{"victim-id": 19, "victim-type": "critical"}`                                                           |
| data.jag.active_duration           | number | Active duration of the joint activity in milliseconds                                          | `4966.0`                                                                                                 |
| data.jag.joint_activity_efficiency | number | Ratio of lower bound estimate over active duration - [0; 1]                                    | `0`, `0.34243`, `0.70933`, `1`                                                                           |
| data.jag.redundancy_ratio          | number | Ratio of active_duration over redundant time - [0; 1]                                          | `0`, `0.25845`, `0.82341`, `1`                                                                           |

## Message Example

### Summary

```json
{
  "header": {
    "timestamp": "2022-04-05T23:05:35.987408Z",
    "message_type": "event",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "Event:Summary",
    "version": "1.2.8",
    "source": "ac_ihmc_ta2_joint-activity-interdependence",
    "timestamp": "2022-04-05T23:05:35.987408Z",
    "experiment_id": "9d6fa759-91de-469b-98ed-672353554fa1",
    "trial_id": "74a9cadc-f892-4be1-a241-b6a645ba477b"
  },
  "data": {
    "jag": {
      "instances": {
        "P000463": "73fc0e47-b09e-47de-badd-8ca3e42e6ee2",
        "P000464": "dd28a91d-d3d0-4b51-a21f-f1ba624e8262",
        "P000465": "68db96c9-87d4-4d78-af1a-31f83f3ab66f"
      },
      "inputs": {
        "victim-id": 19,
        "victim-type": "critical"
      },
      "urn": "urn:ihmc:asist:rescue-victim"
    },
    "active_duration": 27109.0,
    "joint_activity_efficiency": 0.0007208714915031132,
    "redundancy_ratio": 3.688811833713057e-05
  }
}
```
