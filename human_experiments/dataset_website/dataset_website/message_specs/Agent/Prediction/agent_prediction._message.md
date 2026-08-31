# Agent Prediction Message Header Format

Agents should use this message to publish predictions for purposes of evaluation and/or sharing with other agents.
 

## TOPIC

agent/prediction/[ action | state ]/<unique_agent_name>

## Message Fields

| Field Name | Type | Description
| --- | --- | --- |
| header | object | From Common Message Format section
| msg | object | From Common Agent Message Format section
| msg.sub_type | string | The sub_type is prediction with multiple options [action, state]
| data.group | json object | this set of properties apply to all of the predictions in the array belonging to the group.  If a property appears in the prediction array and is different than the corresponding property in the group array, the child property will override the group property for that item.  A prediction message can only contain one group.
| data.created_elapsed_time | number | The time the prediction was created by the agent based on the trial elapsed time
| data.group.start_elapsed_time | number | The time the prediction group is effective in trial elapsed time. if this field is null the prediction will be effective as soon as it is published.
| data.group.duration | number | The length of time in seconds the prediction group remains valid
| data.group.explanation | json object as string | an agent custom json object to describe why this prediction was generated
| msg.sub_type=prediction:action | |
| List of predictions| |
| data.predictions[n].unique_id | string | a unique id for this prediction.  This can be referenced by other messages
| data.predictions[n].start_elapsed_time | number | the time the prediction is made. This should use trial elapsed time.
| data.predictions[n].duration | number | The duration is seconds that the prediction remains valid. If this field is left null, the prediction will be valid for the full trial run
| data.predictions[n].predicted_property | string | the name of the prediction
| data.predictions[n].action | string | the type of action that is being predicted
| data.predictions[n].using | string | the tool that is used in the action
| data.predictions[n].subject | string | the entity taking the action
| data.predictions[n].object | string | who or what is being acted upon
| data.predictions[n].probability_type | string | string or float for the data type of probability
| data.predictions[n].probability | [number \| string ] | a floating point number or a string representation of the probability of this action occurring
| data.predictions[n].confidence_type | string | string or float for the data type of confidence
| data.predictions[n].confidence | [number \| string ] | a floating point number or a string representation of the confidence of this action occurring
| data.predictions[n].explanation | json object as string | an explanation of the the prediction
| End List of | |
| msg.sub_type=prediction:state | |
| List of prediction | |
| data.predictions[n].unique_id | string | a unique id for this prediction.  This can be referenced by other messages
| data.predictions[n].start_elapsed_time | number | the time the prediction becomes valid. If this field is left as null, the prediction will be effective immediately. This is useful if the prediction is made before the mission effectively starts. Use trial elapsed time
| data.predictions[n].duration | number | The duration is seconds that the prediction remains valid. If this field is left null, the prediction will be valid for the full trial run
| data.predictions[n].subject_type | string | the type of the subject either team or individual ["team" \| "individual"]
| data.predictions[n].subject | string | the subject for whom the prediction is being made. This could be a player, a victim, or the full team as a whole
| data.predictions[n].predicted_property | string | the discrete property for which the prediction is made -> for a player this could be position or score, for a victim this could be poisition or triage state, for a team this could be the team score       
| data.predictions[n].prediction | string | the actual predicted value for the subject property in question -> ie 770 for a team score, successful triage for a victim, Location -2100 60 59 for player location
| data.predictions[n].probability_type | string | The format of the probability, as a percent, a float, or some other form
| data.predictions[n].probability | [number \| string ] | The value of the prediction as a string or a float
| data.predictions[n].confidence_type | string | string or float for the data type of confidence
| data.predictions[n].confidence | [number \| string ] | a floating point number or a string representation of the confidence of this action occurring
| data.predictions[n].explanation | json object as string | an explanation of the the prediction
| End List of | |

## Study Measure Names
| Measure Name | Introduced in Study | Prediction Type | Description | Field Details
| --- | --- | --- | --- | --- |
| M1:team_performance | 2 | Prediction:State | Prediction of the final team score | <ul><li> **start_elapsed_time**: trial elapsed_milliseconds time that the prediction was made.  Should be at 4 (240000ms), 9 (540000ms) and 14 (840000ms) minutes <li>**subject_type**: "team" <li>**subject**: the team identifier e.g. "TM000122" <li>**predicted_property**: "M1:team_performace"<li>**prediction**: the predicted final team score as an integer|
| M3:participant_map | 2 | Predcition:State | Prediction of the map displayed to the participant |  <ul><li> **start_elapsed_time**: trial elapsed_milliseconds time that the prediction was made.  Should be at 2 (120000ms), 7 (420000ms) and 12 (720000ms) minutes <li>**subject_type**: "individual" <li>**subject**: the participant_id for the predicted individual <li>**predicted_property**: "M3:participant_map"<li>**prediction**: the predicted map name for this participant.  ["SaturnA_24" \| "SaturnA_34" \| "SaturnA_64" \| "SaturnB_24" \| "SaturnB_34" \| "SaturnB_64"] |
| M6:participant_block_legend | 2 | Prediction:State | Prediction of the marker block legend displayed to the participant | <ul><li> **start_elapsed_time**: trial elapsed_milliseconds time that the prediction was made.  Should be at 3 (180000ms), 8 (480000ms) and 13 (780000ms) minutes <li>**subject_type**: "individual" <li>**subject**: the participant_id for the predicted individual <li>**predicted_property**: "M6:participant_block_legend"<li>**prediction**: the predicted marker block legend for this participant.  ["A_Anne" \| "B_Sally"] |
| M7:participant_room_enter | 2 | Prediction:Action | Prediction of if a participant will enter a room based on a marker block from another participant | <ul><li> **start_elapsed_time**: trial elapsed_milliseconds time that the prediction was made. For this measure it should be when the participant enters the marker block region <li>**predicted_property**: "M7:room_enter"<li> **action**: ["will_enter_room" \| "will_not_enter_room"] <li> **using**: "location, call sign of the participant who placed the block and type of the marker block: {"location": {"x": -600, "y": 120, "z": 60}, "callsign": "Red", "type": "Marker Block 1"}" <li> **subject**: The participant id <li> **object**: "door entered: door id e.g.: c_-9_-25_-8_-24" </li><ul>|

## Study 2 message example - Prediction:State
```json
{
  "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "agent",
    "version": "0.6"
  },
  "msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2019-12-26T14:05:02.1412Z",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "replay_root_id": "123e4567-e89b-12d3-a456-426655440000",
    "source": "tom_generator:1.0",
    "sub_type": "Prediction:State",
    "version": "0.4"
  },
  "data": {
    "created_elapsed_time": 123045,
    "group": {
      "start_elapsed_time": 124034,
      "duration": 30,
      "explanation": "<json object as string>"
    },
    "predictions": [
      {
      "unique_id": "a441d0f7-b54a-4743-8a21-0c763de62f75",
      "start_elapsed_time": 24543,
      "subject_type": "team",
      "subject": "TM000188",
      "predicted_property": "M1:team_performance",
      "prediction": "315"
      },
      {
      "unique_id": "a441d0f7-b54a-4433-8a21-0c763de62f75",
      "start_elapsed_time": null,
      "subject_type": "individual",
      "subject": "E000101",
      "predicted_property": "M3:participant_map",
      "prediction": "SaturnA_24"
      },
      {
      "unique_id": "a441d0f7-b54a-4743-ab12-0c763de62f75",
      "start_elapsed_time": null,
      "subject_type": "individual",
      "subject": "E000102",
      "predicted_property": "M3:participant_map",
      "prediction": "SaturnA_24"
      },
      {
      "unique_id": "a441d0f7-b54a-4743-8a21-0c763cf462f75",
      "start_elapsed_time": 24543,
      "subject_type": "individual",
      "subject": "E000103",
      "predicted_property": "M3:participant_map",
      "prediction": "SaturnA_64"
      },
      {
      "unique_id": "a4e580f7-b54a-4743-8a21-0c763de62f75",
      "start_elapsed_time": 24543,
      "subject_type": "individual",
      "subject": "E000101",
      "predicted_property": "M6:participant_block_legend",
      "prediction": "A_Anne"
      },
      {
      "unique_id": "b711d0f7-b54a-4743-8a21-0c763de62f75",
      "start_elapsed_time": 24543,
      "subject_type": "individual",
      "subject": "E000102",
      "predicted_property": "M6:participant_block_legend",
      "prediction": "A_Anne"
      },
      {
      "unique_id": "a441d0f7-b54a-4743-8a21-0c763de6c515",
      "start_elapsed_time": 24543,
      "subject_type": "individual",
      "subject": "E000103",
      "predicted_property": "M6:participant_block_legend",
      "prediction": "B_Sally"
      }
    ]
  }
}
```
## Study 2 message example - Prediction:Action
```json
{
  "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "agent",
    "version": "0.6"
  },
  "msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2019-12-26T14:05:02.1412Z",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "replay_root_id": "123e4567-e89b-12d3-a456-426655440000",
    "source": "tom_generator:1.0",
    "sub_type": "Prediction:Action",
    "version": "0.4"
  },
  "data": {
    "created_elapsed_time": 543198,
    "group": {
      "start_elapsed_time": null,
      "duration": 30,
      "explanation": "<json string>"
    },
    "predictions": [
      {
        "unique_id": "a441d0f7-b54a-4743-8a21-0c763de62f75",
        "start_elapsed_time": 552285,
        "duration": 30,
        "predicted_property": "M7:room_enter",
        "action": "will_enter_room",
        "using": {"location": {"x": -600, "y": 120, "z": 60}, "callsign": "Red", "type": "Marker Block 1"},
        "subject": "E000199",
        "object": "c_-9_-25_-8_-24",
        "probability_type": "string",
        "probability": "low",
        "confidence_type": "float",
        "confidence": 0.9,
        "explanation": "<json sting>"
      }
    ]
  }
}
```

## Message Example

```json
{ 
  "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "agent",
    "version": "0.6"
  },
  "msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2019-12-26T14:05:02.1412Z",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "replay_root_id": "123e4567-e89b-12d3-a456-426655440000",
    "source": "tom_generator:1.0",
    "sub_type": "Prediction:Action",
    "version": "0.4"
  },
  "data": {
    "created": "2019-12-26T14:05:02.3412Z",
    "group": {
      "start_elapsed_time": 632964,
      "duration": 50,
      "explanation": "{\"explanation\": \"this is an explanation of this prediction\"}"
      },
    "predictions": [
        {
          "unique_id": "a441d0f7-b54a-4743-8a21-0c763de62f75",
          "start_elapsed_time": 749023,
          "duration": 30,
          "predicted_property": "<unique prediction name>",
          "action": "<action>",
          "using": "<using>",
          "subject": "E000231",
          "object": "<object>",
          "probability_type": "float",
          "probability": 0.9,
          "confidence_type": "float",
          "confidence": 0.9,
          "explanation": "{\"extra_agent_data\":{\"key0\":\"data0\",\"key1\":{\"something\":\"some metadata\"},\"key2\":{\"somethingelse\":\"some more metadata\"}"
        },
        {
          "unique_id": "a441d0f7-b54a-4743-8a21-0c763de62f75",
          "start_elapsed_time": 830951,
          "duration": 30,
          "predicted_property": "victim_triage",
          "action": "triage",
          "using": "medkit",
          "subject": "E000413",
          "object": "victim_213",
          "probability_type": "string",
          "probability": "low",
          "confidence_type": "float",
          "confidence": 0.9,
          "explanation": "{\"extra_agent_data\":{\"key0\":\"data0\",\"key1\":{\"something\":\"some metadata\"},\"key2\":{\"somethingelse\":\"some more metadata\"}"
        }
    ]
  }
}

{ 
  "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "agent",
    "version": "0.6"
  },
  "msg": {
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2019-12-26T14:05:02.1412Z",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75",
    "replay_root_id": "123e4567-e89b-12d3-a456-426655440000",
    "source": "tom_generator:1.0",
    "sub_type": "Prediction:State",
    "version": "0.4"
  },
  "data": {
    "created_elapsesd_time": 259846,
    "group": {
      "start_elapsed_time": 270512,
      "duration": 50,
      "explanation": "{\"key0\":\"data0\",\"key1\":\"some metadata\"}"  
    },
    "predictions": [
      {
        "unique_id": "a441d0f7-b54a-4743-8a21-0c763de62f75",
        "subject_type": "individual",
        "subject":"Player447", 
        "predicted_property":"Map Version",
        "prediction":"VERSION A",    
      },
      {
        "unique_id": "a441d0f7-b54a-4743-8a21-0c763de62f75",
        "start_elapsed_time":234512,
        "duration": 20,
        "subject_type": "team",
        "subject":"TM000142", 
        "predicted_property":"total_vitims",
        "prediction":"34",    
        "probability_type":"float",
        "probability_value":0.6, 
        "confidence_type": "float",
        "confidence": 0.9,    
        "explanation":"{\"extra_agent_data\":{\"key0\":\"data0\",\"key1\":{\"something\":\"some metadata\"},\"key2\":{\"somethingelse\":\"some more metadata\"}"      
      }
    ]
  }
}

```
## Version Change History
 
0.4 - changes to support study 2: study specific example and property names
0.3 - added unique id and confidence values
0.2 - Refactored a number of fields. 4/28/2021
0.1 - Initial Creation

