# The Qualtrics Survey response Message Schema
This message includes a survey response from Qualtrics.

## TOPIC
status/asistdataingester/surveyresponse

## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section
| data | object | The object contains the survey response using a Qualtrics specific JSON schema. See also Qualtrics API documentation.

## Additional Qualtrics documentation on responses
https://api-test.qualtrics.com/docs/publicapidocs/reference/singleResponses.json/paths/%7E1surveys%7E1%7BsurveyId%7D%7E1response-schema/get

Responses also contain a mapping table on the "mappings" property between QIDs and more descriptive DataExportTags and QuestionTexts.

## Message Examples
```json
{ "header": {
    "timestamp": "2021-02-10T14:05:02.3412Z",
    "message_type": "status",
    "version": "0.1"
    },
  "msg": { 
    "experiment_id": "523e4567-e89b-12d3-a456-426655440000",
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
    "timestamp": "2021-02-10T14:05:02.3412Z",
    "source": "asistdataingester",
    "sub_type": "Status:SurveyResponse",
    "version": "0.4"},    
  "data" : {
    "responseId": "R_2Y62vxvBCVIJxEG",
    "values": {
      "startDate": "2021-03-25T17:16:56Z",
      "endDate": "2021-03-25T17:21:25Z",
      "status": 0,
      "ipAddress": "165.225.48.92",
      "progress": 100,
      "duration": 269,
      "finished": 1,
      "recordedDate": "2021-03-25T17:21:25.942Z",
      "_recordId": "R_2Y62vxvBCVIJxEG",
      "locationLatitude": "38.8990020751953125",
      "locationLongitude": "-77.19190216064453125",
      "distributionChannel": "anonymous",
      "userLanguage": "EN",
      "QID1_TEXT": "wedw",
      "QID3": 5,
      "QID2": ["2"]
    },
    "labels": {
      "status": "IP Address",
      "finished": "True",
      "QID3": "Slightly uncomfortable",
      "QID2": ["Tuesday"]
    },
    "displayedFields": [
      "QID4_3",
      "QID4_4",
      "QID3",
      "QID4_5",
      "QID1_TEXT",
      "QID2",
      "QID4_1",
      "QID4_2"
    ],
    "displayedValues": {
      "QID4_3": [1, 2, 3, 4, 5],
      "QID4_4": [1, 2, 3, 4, 5],
      "QID3": [1, 2, 3, 4, 5, 6, 7],
      "QID4_5": [1, 2, 3, 4, 5],
      "QID2": ["1", "2", "3", "4", "5", "6", "7"],
      "QID4_1": [1, 2, 3, 4, 5],
      "QID4_2": [1, 2, 3, 4, 5]
    }
  }
}
```
