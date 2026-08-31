# Trial Message Format
A trial message describes the characteristics of a trial.  

## TOPICS

trial

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| id | number | The database id of the trial.
| trial_id | string | The uuid string for the trial.
| name | string | A user friendly name for the trial.
| date | string | The date and time the trial was run.
| experimenter | string | A name of the experimenter performing the trial.
| subjects | string array | A list of the names or ids of the subjects in the trial.
| trial_number | string | Sequentially numbered trial run.
| group_number | string | Data organization identifier.
| study_number | string | Study identifier.
| condition | string | The experimental condition used for the trial.
| notes | string array | A list of notes for the trial.
| testbed_version | string | The testbed version used for the trial.
| experiment | object | The experiment associated with the trial.

## Message Examples
```json
{
  "id": "1",
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
    "id": "1",
	"experiment_id": "946d4567-ab65-cfe7-b208-426305dc1234";
	"name": "Experiment 5",
	"date": "2019-12-26T15:01:00.325Z",
	"author": "Scientist Name",
	"mission": "mdf_load_singleplayer.py"
  }
}
```