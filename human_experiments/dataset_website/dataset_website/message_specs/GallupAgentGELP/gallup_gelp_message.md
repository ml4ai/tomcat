# Gallup - Emergent Leadership Prediction (GELP) - Event Message Format

## About
The Gallup - Emergent Leadership Prediction (GELP) Agent publishes data messages of topic "agent/gelp", subtype "agent: gallup/agent/gelp" to the message bus for reference and use by other agents.

Published are emergent leadership score predictions for participants during the trial.

Scores represent predictions of the likely final scores calculated from similar Gallup Emergent Leadership sections of the trial survey (separately administered), and feature both an overall score and scores for each of the 8 components.

Prediction scores will be issued at roughly each elapsed minute of the trial following trial start.

Predictions are based on an assortment of audio transcription, NLP, competency scores, pre-trial survey info, and other relevant message types subscribed to on the general message bus.

For more information, please refer to the README.md file within the repository folder [Agents/gallup_agent_gelp](https://gitlab.com/artificialsocialintelligence/study3/-/tree/main/Agents/gallup_agent_gelp).

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section 
| msg | object | From Common Event Message Format section
| data.gelp_msg_id | string | Unique message id
| data.gelp_pub_minute | integer | Minute of trial GELP calculation is being created, based on data received up to that point
| data.created_ts | string | Timestamp of GELP calculation, format ISO 8601
| data.gelp_results | object | Contains GELP overall and component calculations per current trial participant(s)
| data.gelp_results.participant_id | string  | The participant_id
| data.gelp_results.callsign | string | The participant callsign for trial
| data.gelp_results.gelp_overall | float | The Gallup - Emergent Leadership Prediction (GELP) overall score
| data.gelp_results.gelp_overall_lower_bound | float | The Gallup - Emergent Leadership Prediction (GELP) overall score, lower bound
| data.gelp_results.gelp_overall_upper_bound | float | The Gallup - Emergent Leadership Prediction (GELP) overall score, upper bound
| data.gelp_results.gelp_components | list of float | The Gallup - Emergent Leadership Prediction (GELP) Components object: Overall Scores by Component where index 0 equates to component 1, etc.
| data.gelp_results.gelp_components_lower_bound | list of float | The Gallup - Emergent Leadership Prediction (GELP) Components object: Lower Bound Scores by Component where index 0 equates to component 1, etc.
| data.gelp_results.gelp_components_upper_bound | list of float | The Gallup - Emergent Leadership Prediction (GELP) Components object: Upper Bound Scores by Component where index 0 equates to component 1, etc.
| data.gelp_results.gelp_missingness_factor | float | Ratio of unavailable participant feature columns vs. model-expected feature columns at time of calculation. Lower result indicates less imputation of missing data in model at time of calculation.

## TOPIC
agent/gelp

## Message Example

```json
{
	"header": {
		"timestamp": "2021-12-12T22:03:27.278274Z",
		"version": "0.6.0",
		"message_type": "agent"
	},
	"msg": {
		"sub_type": "agent:gallup_agent_gelp",
		"timestamp": "2021-12-12T22:03:27.278274Z",
		"experiment_id": "[supplied outside of agent]",
		"trial_id": "[supplied outside of agent]",
		"version": "0.5.0",
		"source": "gallup_agent_gelp"
	},
	"data": {
		"gelp_msg_id": "3f42e519-85eb-49a3-b811-745525cba829",
		"gelp_pub_minute": 13,
		"created_ts": "2021-12-12T22:03:27.277182Z",
		"gelp_results": [
			{
				"participant_id": "E000997",
				"callsign": "Red",
				"gelp_overall": 4.933666666666667,
				"gelp_overall_lower_bound": 1.8827446685571197,
				"gelp_overall_upper_bound": 7.984588664776215,
				"gelp_components": [
					3.0796666666666665,
					3.348833333333333,
					3.902333333333333,
					3.243,
					3.6355,
					3.2348333333333336,
					3.878666666666667,
					4.379833333333333
				],
				"gelp_components_lower_bound": [
					0.7582169230855591,
					0.6692644955419644,
					1.7675072490766613,
					0.44813723778933226,
					1.4654631636720064,
					0.7875101456930285,
					1.5796453806297604,
					2.2541544388080139
				],
				"gelp_components_upper_bound": [
					5.401116410247774,
					6.028402171124702,
					6.037159417590004,
					6.0378627622106679,
					5.8055368363279939,
					5.682156520973638,
					6.1776879527035739,
					6.505512227858652
				],
				"gelp_missingness_factor": 0.8813559322033898,
			},
			{
				"participant_id": "E000998",
				"callsign": "Green",
				"gelp_overall": 5.646666666666667,
				"gelp_overall_lower_bound": 2.5957446685571199,
				"gelp_overall_upper_bound": 8.697588664776216,
				"gelp_components": [
					3.6746666666666676,
					3.7173333333333327,
					4.348333333333333,
					3.749,
					3.954,
					3.7768333333333325,
					4.126666666666668,
					4.704833333333332
				],
				"gelp_components_lower_bound": [
					1.3532169230855602,
					1.037764495541964,
					2.213507249076661,
					0.9541372377893325,
					1.7839631636720066,
					1.3295101456930275,
					1.827645380629761,
					2.579154438808013
				],
				"gelp_components_upper_bound": [
					5.996116410247774,
					6.396902171124701,
					6.483159417590004,
					6.543862762210668,
					6.124036836327994,
					6.224156520973637,
					6.425687952703575,
					6.830512227858652
				],
				"gelp_missingness_factor": 0.7355932203389823,
			},
			{
				"participant_id": "E000999",
				"callsign": "Blue",
				"gelp_overall": 3.592,
				"gelp_overall_lower_bound": 0.5410780018904524,
				"gelp_overall_upper_bound": 6.642921998109548,
				"gelp_components": [
					2.304,
					2.542,
					3.119,
					2.2935,
					2.9105,
					2.2975,
					2.8935,
					3.404
				],
				"gelp_components_lower_bound": [
					-0.017449743581107403,
					-0.13756883779136865,
					0.9841739157433285,
					-0.5013627622106678,
					0.7404631636720063,
					-0.1498231876403051,
					0.5944787139630932,
					1.2783211054746806
				],
				"gelp_components_upper_bound": [
					4.6254497435811079,
					5.221568837791368,
					5.253826084256672,
					5.088362762210668,
					5.080536836327994,
					4.744823187640305,
					5.192521286036907,
					5.529678894525319
				],
				"gelp_missingness_factor": 0.9915254237288136
			}
		]
	}
}
```

Please note document draft status. Revisions are anticipated, above information and examples are not yet final and subject to change.

Revision Notes:
[2022-05-12] Updated schema to reflect current version output (0.6.0)
[2022-02-22] Updated schema to reflect current version output (0.5.2)
[2022-03-01] Updates to schema to reflect latest version (0.5.1), addition of "missigness_factor" to per-participant output in message field and message example sections.
[2022-02-22] Updated schema to reflect current version output (0.5.0)
[2021-12-13] Updated schema to reflect current version output (removed precision parameter for scores in JSON schema: "multipleOfPrecision": 0.001)
