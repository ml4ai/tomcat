## TOPIC

agent/speech_analyzer/personality

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
| data.personality     | object | Personality label for an utteranced calculated from vocalic features |

## Message Example

```json
{
  "data": {
    "utterance_id": "59678a5f-9c5b-451f-8506-04bc020f2cf3",
    "personality": {
	"traits": {
			"agreeableness": 0.5746405124664307,
			"conscientiousness": 0.1577334851026535,
			"extroversion": 0.017741311341524124,
			"neuroticism": 0.03425062820315361,
			"openness": 0.2156340479850769
		},
	"penultimate_traits": [0.010533388704061508, -0.0, -0.0, 0.0, 1.8938840627670288, -0.0, 0.0, 0.0, 0.0, -0.0, 0.0, 1.3441832065582275, -0.0, 1.1930299997329712, 0.8612010478973389, 0.0, 0.0, 0.34098416566848755, 0.34329915046691895, 0.0, 0.0, 0.0, -0.0, 2.2876157760620117, 1.2375566959381104, 0.4342152178287506, 0.0, 0.004195062443614006, 0.7166615724563599, 2.2854552268981934, 0.0, 1.6064200401306152, 0.0, 0.3799717128276825, 0.26557832956314087, -0.0, 0.0, -0.0, 0.0, -0.0, 0.0, 0.0, 1.6425988674163818, -0.0, -0.0, 1.3372029066085815, 0.0, -0.0, 0.0, 1.267110824584961],
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
    "sub_type": "speech_analyzer:personality"
  }
}
```

