# Control Message Format
A control message is sent to all components on the message bus to inform them of general actions in the testbed. 
 These actions provide for initializing testbed components, Starting, pausing or stopping an experiment and replaying previously captured data. 

 ## TOPIC

control 

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| header | object | From Common Message Format section
| msg.command | string | Optional command such as start, stop, replay
| msg.date | string | date of action
| msg.experiment_id | string | experiment id
| msg.trial_id | string | The experiment trial id of the trial being run.
| msg.replay_id | string | If the original trial data was replayed, this field indicates a unique uuid for the replay
| msg.experimenter | string | the name of the experimenter
| msg.name | string | A user friendly name for the trial
| msg.subjects | string array | A list of the ids of the subjects in the trial
| msg.testbed | string | version of the testbed used

## Message Examples
```json
{ "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "control",
    "version": "0.5"
    },
"msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "command": "init",
    "experiment_name": "Engineering test 1"
  }
}
```
This example is for trial start published on the trial/start message bus topic.
```json
{ "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "control",
    "version": "0.5"
    },
"msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"date": "2019-12-26T14:07:05.3257Z",
	"experiment_id": "427e4567-e8db-13d3-a841-426655445926"
    "experiment_name": "Engineering test 1"
	"experimenter": "Scientist Name",
	"name": "Experiment 5 trial 2",
	"subjects": ["s312", "s429", "s12", "s734"],
	"testbed": "0.5"
  }
}
```

```json
{ "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "control",
    "version": "0.5"
    },
"msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"replay_id": "876e4567-ab65-cfe7-b208-426305dc1234",
    "command": "replay",
    "experiment_name": "Engineering test 1",
    "start_time": "2019-12-26T12:43:16.0000Z"
  }
}
```