## TOPIC

agent/speech_analyzer/sentiment

## Message Fields

| Field Name          | Type    | Description                                                                              |
| ---                 | ---     | ---                                                                                      |
| header.timestamp    | string  | Timestamp of when the data was generated in ISO 8601 format: YYYY-MM-DDThh:mm:ss.sssz    |
| header.message_type | string  | One of the defined message types                                                         |
| header.version      | string  | The version of the message type object                                                   |
| msg.timestamp       | string  | Timestamp of when the data was generated in ISO 8601 format: YYYY-MM-DDThh:mm:ss.sssz    |
| msg.experiment_id   | string  | The experiment id this message is associated with                                        |
| msg.trial_id        | string  | The trial id this message is associated with                                             |
| msg.version         | string  | The version of the sub_type format                                                       |
| msg.source          | string  | The name of the component that published this data                                       |
| msg.sub_type        | string  | The subtype of the data. This field describes the format of this particular type of data |
| data.id             | string  | A version 4 UUID associated with this message                                                                        |
| data.utterance_id             | string  | A version 4 UUID used to link asr and sentiment messages                                                                        |
| data.sentiment     | object | Sentiment label for an utteranced calculated from vocalic features |

## Message Example

```json
{
  "data": {
    "utterance_id": "59678a5f-9c5b-451f-8506-04bc020f2cf3",
    "sentiment": {
      "emotions": {
        "anger": 0.002021155087277293,
        "disgust": 0.09554211795330048,
        "fear": 0.002021155087277293,
        "joy": 0.022178679704666138,
        "neutral": 0.87403804063797,
        "sadness": 0.002021155087277293,
        "surprise": 0.0021777180954813957
      },
      "penultimate_emotions": [0,0.20094694197177887,0,0.5490803718566895,1.1174331903457642,-0,0,0,1.2479413747787476,0,1.2241981029510498,1.6163171529769897,0,1.5434015989303589,3.1412456035614014,1.9181157350540161,3.3949952125549316,2.7259912490844727,0,0.24653026461601257,0,1.9409351348876953,0,0.853614091873169,0,-0,0,1.2603272199630737,2.613118886947632,0,0,-0,0,0,0,1.7558680772781372,1.7072715759277344,0,0,0,1.715401530265808,2.9527125358581543,0,1.9544548988342285,0,2.6578049659729004,0,0,0,-0]
    }
  },
  "header": {
    "timestamp": "2021-01-19T23:27:58.633076Z",
    "message_type": "observation",
    "version": "0.1"
  },
  "msg": {
    "timestamp": "2021-01-19T23:27:58.633967Z",
    "experiment_id": "e2a3cb96-5f2f-11eb-8971-18810ee8274e",
    "trial_id": "256d1b4a-d81d-465d-8ef0-2162ff96e204",
    "version": "3.3.2",
    "source": "tomcat_speech_analyzer",
    "sub_type": "speech_analyzer:sentiment"
  }
}
```

