# Experiment Message Format
An experiment message describes the characteristics of an experiment.  

## TOPIC

experiment

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| header | object | From the Common Message Format section.
| msg | object | From the Common Message Format section.
| data.name | string | A user friendly name for the experiment.
| data.date | string | The date and time the experiment was created.
| data.author | string | The name of the author of the experiment.
| data.mission | string | The mission associated with this experiment.

## Message Examples
```json
{
  "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "experiment",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "create",
    "source": "gui",
    "experiment_id": "946d4567-ab65-cfe7-b208-426305dc1234",
    "timestamp": "2019-12-26T15:01:00.325Z",
    "version": "1.1"
  },
  "data": {
    "name": "Experiment 5",
    "date": "2019-12-26T15:01:00.325Z",
    "author": "Scientist Name",
    "mission": "mdf_load_singleplayer.py"
  }
}
```
## Version Change History
 
1.1 | 1/12/2021 | Added "msg" and "header" fileds to conform with all other messages 

1.0 