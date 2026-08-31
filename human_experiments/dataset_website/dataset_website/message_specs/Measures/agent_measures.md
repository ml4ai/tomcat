# agent subtype : measures Message Format
The measures published by various agents.  

## TOPICS

agent/measures/+
(agent/measures/AC_Aptima_TA3_measures)


## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| header | object (required)| From the Common Header Format section.
| msg | object (required)| From the Common Message Format section.
| data.study_version | number (required)| the current study.
| data.elapsed_milliseconds | number (required)| the current elapsed milliseconds.
| data.event_properties | object (required)| data which describes the event which triggers the measure.
| data.event_properties.qualifying_event_type | string (required)| the event type one of ["event", "timedEvent", "time"].
| data.event_properties.qualifying_event_message_type | string | the messagetype that triggered the measure calculation if qualifying_event_type is: "event" or "timedEvent".
| data.event_properties.qualifying_event_sub_type | string | the message subtype that triggered the measure caclulation if qualifying_event_type is: "event" or "timedEvent".  For example: "Event:VictimEvacuated", "Event:MissionState".
| data.event_properties.time_delta | number | the time delta between the event that caused the measure to be calculated and the actual time the measure is calculated if qualifying_event_type is "timedEvent" In elapsed_milliseconds. For example 2000 milli-seconds after the transporter signal triggers.
| data.event_properties.mission_time | number | A specific mission time in elapsed_milliseconds when the measure calculation was done if qualifying_event_type is "time".  For example 300000 milliseconds (5 minutes) into the mission.
| data.measure_data | object array (required)| an array of data for the current measure.
| data.measure_data.datatype | string (required)| the datatype of the current measure.
| data.measure_data.measure_id | string (required)| the id of the current measure.
| data.measure_data.measure_value | any (required)| the value of the current measure.
| data.measure_data.description | string (required)| a description of the current measure.
| data.measure_data.additional_data | object | user defined object with any addiotional data needed.


## Event Message Example
```json
{
  "header" : {
    "timestamp" : "2021-05-27T16:23:36.1317Z",
    "version" : "3.0",
    "message_type" : "agent"
  },
  "msg" : {
    "sub_type" : "measures",
    "timestamp" : "2021-05-27T16:23:36.1317Z",
    "experiment_id" : "730527c7-c06d-4edf-906c-ac3996460faf",
    "trial_id" : "97e409cc-e1e1-4698-9154-14358d98f55f",
    "version" : "3.0",
    "source" : "AC_Aptima_TA3_measures"
  },
  "data" : {
    "study_version": "3",
    "elapsed_milliseconds": 927927,
    "event_properties": {
      "qualifying_event_type": "event",
      "qualifying_event_message_type": "Event",
      "qualifying_event_sub_type": "Event:VictimEvacuated",
      "time_delta": null,
      "mission_time": null
    },
    "measure_data" : [
      {
        "measure_id": "ASI-M3",
        "datatype": "double",
        "measure_value": 0.67,
        "description": "Error Rate",
        "additional_data": {
          "victims_rescued": 7,
          "victims_discovered": 12
        }      
      }
    ]
  }
}
```

## Timed Message Example
```json
{
  "header" : {
    "timestamp" : "2021-05-27T16:23:36.1317Z",
    "version" : "3.0",
    "message_type" : "agent"
  },
  "msg" : {
    "sub_type" : "measures",
    "timestamp" : "2021-05-27T16:23:36.1317Z",
    "experiment_id" : "730527c7-c06d-4edf-906c-ac3996460faf",
    "trial_id" : "97e409cc-e1e1-4698-9154-14358d98f55f",
    "version" : "3.0",
    "source" : "AC_Aptima_TA3_measures"
  },
  "data" : {
    "study_version": "3",
    "elapsed_milliseconds": 240000,
    "event_properties": {
      "qualifying_event_type": "time",
      "qualifying_event_message_type": "",
      "qualifying_event_sub_type": "",
      "time_delta": null,
      "mission_time": 240000
    },
    "measure_data" : [
      {
        "measure_id": "M123T",
        "datatype": "integer",
        "measure_value": 50,
        "description": "Current score",
        "additional_data": {}      
      }
    ]
  }
}
```
