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
| data.metadata.trial | object | The trial exported.

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
	"metadata": {
	  "trial": {
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
		"experiment_name": "Experiment 5",
		"experiment_date": "2019-12-26T15:01:00.325Z",
		"experiment_author": "Scientist Name",
		"experiment_mission": "mdf_load_singleplayer.py"
	  }
	}
  }
}
```