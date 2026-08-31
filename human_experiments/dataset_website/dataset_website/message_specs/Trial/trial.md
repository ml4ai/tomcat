# Trial Message Format
A trial message describes the characteristics of a trial.  

## TOPICS

trial

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| header | object | From the Common Header Format section.
| msg | object | From the Common Message Format section.
| data.name | string | A user friendly name for the trial.
| data.date | string | The date and time the trial was run.
| data.experimenter | string | A name of the experimenter performing the trial.
| data.subjects | string array | A list of the names or ids of the subjects in the trial.
| data.trial_number | string | Sequentially numbered trial run.
| data.group_number | string | Data organization identifier.
| data.study_number | string | Study identifier.
| data.condition | string | The experimental condition used for the trial.
| data.notes | string array | A list of notes for the trial.
| data.testbed_version | string | The testbed version used for the trial.
| data.experiment_name | string | A user friendly name for the experiment.
| data.experiment_date | string | The date and time the experiment was created.
| data.experiment_author | string | The name of the author of the experiment.
| data.experiment_mission | string | The mission associated with this experiment.
| data.map_name | string | The Map Name.
| data.map_block_filename | string | The Map Block file to use during trial.
| data.intervention_agents | string array | A list of agent names which can show interventions to the users
| data.client_info | object array | client info parameters assigned by experimenter.
| data.role_text | object | role specific text to be displayed on the client map.


## Message Examples
```json
{
  "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "trial",
    "version": "1.1"
  },
  "msg": {
    "sub_type": "start",
    "source": "gui",
    "experiment_id": "946d4567-ab65-cfe7-b208-426305dc1234",
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2019-12-26T15:01:00.325Z",
    "version": "0.4",
	"replay_id": null,
    "replay_parent_id": null,
	"replay_parent_type": null
  },
  "data": {
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
    "experiment_mission": "mdf_load_singleplayer.py",
    "map_name": "Falcon",
    "map_block_filename": "MapBlocks_Easy.csv",
    "intervention_agents": [ "AgentName1" ],
    "client_info" : [ {
      "playername" : "Scouter_B",
      "callsign" : "Alpha",
      "participant_id" : "hi",
      "staticmapversion" : "SaturnA_base",
      "markerblocklegend" : "A_Anne",
      "unique_id" : "id"
      }, {
      "playername" : "testplayer",
      "callsign" : "Bravo",
      "participant_id" : "foo",
      "staticmapversion" : "SaturnA_64",
      "markerblocklegend" : "B_Sally",
      "unique_id" : "id-2"
    } ]
  }
}
```