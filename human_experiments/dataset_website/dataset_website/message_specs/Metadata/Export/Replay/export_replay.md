# Trial Message Format
A trial message describes the characteristics of a trial.  

## TOPICS

trial

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| header | object | From the Common Header Format section.
| msg | object | From the Common Message Format section.
| data.index | string | The elasticsearch index name that has the tiral events and observations.
| data.ignore_list | string array | A lists of types to igonore when exporting.
| data.metadata.parents | object | The parent history of the replay, containing a list of either a Trial or Replay metadata objects.

## Message Examples
```json
{
	"header": {
		"timestamp": "2019-12-26T14:05:02.3412Z",
		"message_type": "export",
		"version": "0.5"
	},
	"msg": {
		"sub_type": "trial",
		"source": "gui",
		"experiment_id": "946d4567-ab65-cfe7-b208-426305dc1234",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T15:01:00.325Z",
		"version": "0.4"
	},
	"data": {
		"index": "",
		"ignore_list": [],
		"metadata": {
			"parents": {
				"trial": {
					"id": 1,
					"trial_id": "123e4567-e89b-12d3-a456-426655440000",
					"name": "Experiment 5 trial 2",
					"date": "2019-12-26T15:01:00.325Z",
					"experimenter": "Scientist Name",
					"subjects": [
						"s312",
						"s429",
						"s12",
						"s734"
					],
					"trial_number": "0001",
					"group_number": "555",
					"study_number": "Study_01",
					"condition": "An experimental condition.",
					"notes": [
						"line1",
						"line2",
						"line3",
						"line4"
					],
					"testbed_version": "0.5",
					"experiment": {
						"id": 1,
						"experiment_id": "946d4567-ab65-cfe7-b208-426305dc1234",
						"name": "Experiment 5",
						"date": "2019-12-26T15:01:00.325Z",
						"author": "Scientist Name",
						"mission": "mdf_load_singleplayer.py"
					}
				},
				"replay": {
					"id": 1,
					"replay_id": "123e4567-e89b-12d3-a456-426655449999",
					"replay_parent_id": "123e4567-e89b-12d3-a456-426655440000",
					"replay_parent_type": "TRIAL",
					"date": "2019-12-26T14:05:02.3412Z",
					"ignore_list": [
						"message_type 1",
						"message_type 2",
						"message_type 3"
					]
				}
			}
		}
	}
}
```