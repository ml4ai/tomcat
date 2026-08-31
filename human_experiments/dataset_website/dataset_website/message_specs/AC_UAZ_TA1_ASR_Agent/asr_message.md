## TOPIC

agent/asr/intermediate : For intermediate transcription results 

agent/asr/final : For final transcription results 

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
| data.text           | string  | The transcription returned from the ASR system                                           |
| data.alternatives   | object array | A list of alterative transcription objects returned from the ASR system             |
| data.is_final       | boolean | Indicates whether the transcription is an intermediate or final transcription            |
| data.is_initial     | boolean | Indicates whether an intermediate message marks the start of an utterance                |
| data.asr_system     | string  | The system used by the agent for automatic speech recognition                            |
| data.id             | string  | A version 4 UUID associated with this message                                                                        |
| data.participant_id | string  | The participant id this data is assosiated with                                          |
| data.start_timestamp | string | A timestamp representing the start of the utterance |
| data.end_timestamp | string | A timestamp representing the end of the utterance |
| data.features      | object | Word alignment for extracted vocalic features |
| data.sentiment     | object | Sentiment label for an utteranced calculated from vocalic features |

## Message Example

```json
{
  "data": {
    "text": "I am going to save a green victim.",
    "alternatives": [
      {
        "text": "I am going to save a green victim.",
        "confidence": 0.981501042842865
      },
      {
        "text": "I am going to save a Greene victim.",
        "confidence": 0.974869549274446
      },
      {
        "text": "I am going to save a grain victim.",
        "confidence": 0.9713254261016846
      },
      {
        "text": "I am going to save a green victum.",
        "confidence": 0.974603054523467
      },
      {
        "text": "I am going to save a green victom.",
        "confidence": 0.979378929138184
      }
    ],
    "asr_system": "Google",
    "is_final": true,
    "is_initial": false,
    "id": "59678a5f-9c5b-451f-8506-04bc020f2cf3",
    "participant_id": "participant_1",
    "start_timestamp": "2021-01-19T23:27:57.978016Z",
    "end_timestamp" : "2021-01-19T23:27:58.633076Z",
    "features" : "See example_asr_message.json",
    "sentiment" : {"speaker":"35cb434e-0c18-44d3-94e3-e532bbfc178c","emotions":{"anger":0.002021155087277293,"disgust":0.09554211795330048,"fear":0.002021155087277293,"joy":0.022178679704666138,"neutral":0.87403804063797,"sadness":0.002021155087277293,"surprise":0.0021777180954813957},"traits":{"extroversion":0.003421018598601222,"neuroticism":0.003421018598601222,"agreeableness":0.6680639386177063,"openness":0.13575837016105652,"conscientiousness":0.18933558464050293},"penultimate_emotions":[0.0,0.20094694197177887,0.0,0.5490803718566895,1.1174331903457642,-0.0,0.0,0.0,1.2479413747787476,0.0,1.2241981029510498,1.6163171529769897,0.0,1.5434015989303589,3.1412456035614014,1.9181157350540161,3.3949952125549316,2.7259912490844727,0.0,0.24653026461601257,0.0,1.9409351348876953,0.0,0.853614091873169,0.0,-0.0,0.0,1.2603272199630737,2.613118886947632,0.0,0.0,-0.0,0.0,0.0,0.0,1.7558680772781372,1.7072715759277344,0.0,0.0,0.0,1.715401530265808,2.9527125358581543,0.0,1.9544548988342285,0.0,2.6578049659729004,0.0,0.0,0.0,-0.0],"penultimate_traits":[1.5018659830093384,-0.0,-0.0,0.0,3.066028356552124,-0.0,0.0,0.0,0.0,-0.0,0.8364737629890442,1.2047011852264404,-0.0,2.4049181938171387,2.264213800430298,0.0,0.0,0.02852160483598709,0.6170178651809692,0.0,0.0,0.0,-0.0,2.739126682281494,2.376819610595703,1.433931589126587,0.0,0.16708949208259583,0.0,3.269641876220703,0.0,2.213355541229248,0.0,0.0,0.872717022895813,-0.0,0.0,-0.0,-0.0,-0.0,0.0,0.0,3.478353500366211,-0.0,-0.0,1.2697429656982422,0.0,-0.0,-0.0,2.093494176864624]}
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
    "source": "speech_analyzer_agent",
    "sub_type": "asr:transcription"
  }
}
```

