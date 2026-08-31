# Trial Message Format
A trial message describes the characteristics of a experiment.  

## TOPICS

trial

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| id | number | The database id of the experiment.
| experiment_id | string | The uuid string for the experiment.
| name | string | A user friendly name for the experiment.
| date | string | The date and time the experiment was created.
| author | string | The name of the author of the experiment.
| mission | string | The mission associated with this experiment.

## Message Examples
```json
{
  "id": "1",
  "experiment_id": "946d4567-ab65-cfe7-b208-426305dc1234";
  "name": "Experiment 5",
  "date": "2019-12-26T15:01:00.325Z",
  "author": "Scientist Name",
  "mission": "mdf_load_singleplayer.py"
}
```