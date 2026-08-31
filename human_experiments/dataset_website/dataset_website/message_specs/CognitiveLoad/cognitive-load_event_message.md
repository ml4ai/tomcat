# Data Subtype: Event:cognitive_load Message Format
This data message subtype, `Event:cognitive_load`, is used to communicate team cognitive load and the probability of forgetting.

## TOPIC

agent/measure/<agent_name>/load

## Message Fields

| Field Name                      | Type    | Description                                                                            
|---------------------------------|---------|----------------------------------------------------------------------------------------|
| header                          | object  | From Common Message Format section                                                     |
| msg                             | object  | From the event Header Format section                                                   |
| data.id                         | string  | Unique Id associated with this event                                                   |
| data.agent                      | string  | The name of the agent who sent this message                                            |
| data.created                    | string  | Timestamp of when the data was generated in ISO 8601 format: YYYY-MM-DDThh:mm:ss.ssssZ |
| data.elapsed_milliseconds       | number  | the number of elapsed milliseconds since mission start                                 |
| data.cognitive_load             | Measure | Cognitive load measure                                                                 |
| data.probability_of_forgetting  | Measure | Probability of forgetting measure                                                      |

## Message Example

```json
{
    "header": {
        "timestamp": "2021-10-11T01:05:29.826595Z",
        "message_type": "event",
        "version": "1.1"
    },
    "msg": {
        "sub_type": "Event:cognitive_load",
        "version": "1.0",
        "source": "AC_CMUFMS_TA2_Cognitive",
        "timestamp": "2021-10-11T01:05:29.826595Z",
        "experiment_id": "39decb46-72fc-4a54-9a78-7571b6011e27",
        "trial_id": "3c6adbb6-ee33-4e7e-8d00-296d27a0d32a"
    },
    "data":     {
        "id": "587b797e-a0f6-4a0d-8769-877b2bd36785",
        "agent": "AC_CMUFMS_TA2_Cognitive",
        "created": "2022-03-10T01:03:54.022834Z",
        "elapsed_milliseconds": 560236,
        "cognitive_load": {
            "value": 4.654498457656448,
            "confidence": 0.5052246336163365
        },
        "probability_of_forgetting": {
            "value": 0.9724250461356305,
            "confidence": 0.04152453959113148
        }
    }
}
```