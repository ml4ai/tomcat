# Trial Message Format
A trial message describes the characteristics of the role_text portion of a trial.  

## TOPICS

trial
## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| mission_name | string | the mission name
| medical_specialist_text | string[] | text to be displayed on the medic client map
| transport_specialist_text | string[] | text to be displayed on the transporter client map
| engineering specialist_text | string[] | text to be displayed on the engineer client map

## TOPIC
ground_truth/mission/role_text

## Message Example

```json
{
	"header": {
		"timestamp": "2019-12-26T12:47:23.1234Z",
		"message_type": "groundtruth",
		"version": "0.6"
	},
	"msg": { 
		"experiment_id": "563e4567-e89b-12d3-a456-426655440000",
		"trial_id": "123e4567-e89b-12d3-a456-426655440000",
		"timestamp": "2019-12-26T14:05:02.1412Z",
		"source": "simulator",
		"sub_type": "Mission:RoleText",
		"version": "2.0"
	},
	"data": {

                "mission_name": "Saturn_A",
                "medical_specialist_text": ["Saturn_A Medical Specialist"],
                "transport_specialist_text": ["Saturn_A Transport Specialist"],
                "engineering_specialist_text": ["Saturn_A Engineering Specialist"]
        
        }

```

VERSION | DATE | DETAILS
| --- | --- | --- |
| 2.1 | 12/3/21 | topic corrected to ground_truth from groundtruth
| 2.0 | 12/3/21 | Initial Spec Created