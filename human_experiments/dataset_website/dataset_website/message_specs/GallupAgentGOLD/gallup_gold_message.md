# Gallup - Object Library Distributor (GOLD) - Event Message Format

## About
The Gallup - Object Library Distributor (GOLD) Agent publishes data messages of topic "agent/gold", subtypes "standard","bullion" to the message bus for reference and use by other agents.

GOLD will publish a variety of small factor, component data messages to the message bus for reference. These will be published respective to participant_id. Published messages will be comprised of various features engineered from raw event data, gleaned from various points along the Gallup data and modeling pipeline.

Message events published by GOLD fall into two sub-types:
- "standard". Immediately upon trial start, GOLD with publish an inventory of all potential features that can be calculated and _may_ be seen in a future event message publication. Feature inventory will be found in the message's data.gold_feature_inventory object. At time of "standard" publication, message's data.gold_results object will be null (empty list).
- "bullion". At each minute-by-minute publication, GOLD will publish available features as calculated based on data aggregated prior to the minute of publication. Feature calculations will be found in the message's data.gold_results. At time of "bullion" publication, message's data.gold_feature_inventory object will be null (empty list).

Items published by GOLD within the "bullion" sub_type fall into two groupings, identified by their prefix.
- "agg_" prefix indicates _aggregate_ features compiled from Automatic Speech Recognition (ASR) speech-to-text transcription and Natural Language Processing (NLP) events as subscribed to from the message bus. Aggregated features represent quantitative, semantic feature counts by rule type as of a given publication time. Example agg_ features include speaking, dominant, and first-word types. Also included in this grouping are features echoing survey responses according to participant, QID.
- "mod_" prefix indicates _modeled_ features are generated from a series of 7 categories of speech models, where the inputs are ASR events and outputs are labelled predictions. Minute-by-minute predictions for each category model are stated according to participant_id and include perspective of the generated score vs. study2 average of cumulative sum for each label. Speech model categories include: Motivation, Compensatory Helping, Contingent Planning, Deliberative Planning, Role Clarification, Reactive Planning, and Transactive Memory.

For more information, please refer to the README.md file within the repository folder [Agents/gallup_agent_gold](https://gitlab.com/artificialsocialintelligence/study3/-/tree/main/Agents/gallup_agent_gold).

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Header Format section 
| msg | object | From Common Event Message Format section
| msg.sub_type | Differentiates between "standard" representing features available, and "bullion" representing data respective to each available feature at time of publication
| msg.version | integer | Current version of the GOLD Agent
| data.gold_msg_id | string | Unique message id
| data.gold_pub_minute | integer | Publication minute during trial
| data.created_ts | string | Timestamp of GOLD component publication, format ISO 8601
| data.gold_feature_inventory | object | List of features available for GOLD to calculate and publish. Not populated when msg.sub_type=bullion.
| data.gold_results | object | GOLD item object payload. Not populated when sub_type=standard. When present, features represented will include respective participant_id, agg_trial, agg_minute along with available feature scoring according to feature inventory list published in sub_type=standard.

## TOPIC
agent/gold

## Message Example, msg.sub_type == "standard"

```json
{
	"header": {"timestamp": "2022-03-10T00:41:15.806266Z", "message_type": "event", "version": "1.1"}, 
	"msg": {
		"sub_type": "standard", 
		"version": "0.2.0", 
		"source": "gallup_ta2_agent_gold", 
		"timestamp": "2022-03-10T00:41:15.806266Z", 
		"experiment_id": "[supplied outside of agent]", 
		"trial_id": "[supplied outside of agent]"}, 
	"data": {
		"gold_msg_id": "2889b738-846b-47a6-93e5-830f25e57e98", 
		"gold_pub_minute": 0, 
		"created_ts": "2022-03-10T00:41:15.804116Z", 
		"gold_feature_inventory": [
			"agg_action_terms", "agg_approval_terms", "agg_avg_num_words", "agg_coordination_terms", "agg_dominant_speaker", "agg_entities_all", "agg_first_word", "agg_last_word", "agg_long_utterance", "agg_minute", "agg_num_utterances", "agg_pc_QID830_1", "agg_pc_QID830_10", "agg_pc_QID830_11", "agg_pc_QID830_12", "agg_pc_QID830_13", "agg_pc_QID830_14", "agg_pc_QID830_15", "agg_pc_QID830_2", "agg_pc_QID830_3", "agg_pc_QID830_4", "agg_pc_QID830_5", "agg_pc_QID830_6", "agg_pc_QID830_7", "agg_pc_QID830_8", "agg_pc_QID830_9", "agg_planning_all", "agg_rme_vars_QID751", "agg_rme_vars_QID753", "agg_rme_vars_QID755", "agg_rme_vars_QID757", "agg_rme_vars_QID759", "agg_rme_vars_QID761", "agg_rme_vars_QID763", "agg_rme_vars_QID765", "agg_rme_vars_QID767", "agg_rme_vars_QID769", "agg_rme_vars_QID771", "agg_rme_vars_QID773", "agg_rme_vars_QID775", "agg_rme_vars_QID777", "agg_rme_vars_QID779", "agg_rme_vars_QID781", "agg_rme_vars_QID783", "agg_rme_vars_QID785", "agg_rme_vars_QID787", "agg_rme_vars_QID789", "agg_rme_vars_QID791", "agg_rme_vars_QID793", "agg_rme_vars_QID795", "agg_rme_vars_QID797", "agg_rme_vars_QID799", "agg_rme_vars_QID801", "agg_rme_vars_QID803", "agg_rme_vars_QID805", "agg_rme_vars_QID807", "agg_rme_vars_QID809", "agg_rme_vars_QID811", "agg_rme_vars_QID813", "agg_rme_vars_QID815", "agg_rme_vars_QID817", "agg_rme_vars_QID819", "agg_rme_vars_QID821", "agg_role_terms", "agg_sa_QID13_1", "agg_sa_QID13_10", "agg_sa_QID13_11", "agg_sa_QID13_12", "agg_sa_QID13_13", "agg_sa_QID13_14", "agg_sa_QID13_15", "agg_sa_QID13_2", "agg_sa_QID13_3", "agg_sa_QID13_4", "agg_sa_QID13_5", "agg_sa_QID13_6", "agg_sa_QID13_7", "agg_sa_QID13_8", "agg_sa_QID13_9", "agg_sd_QID832_16", "agg_sd_QID832_17", "agg_sd_QID832_18", "agg_sd_QID832_19", "agg_sd_QID832_20", "agg_sd_QID832_21", "agg_sd_QID832_22", "agg_sd_QID832_23", "agg_sd_QID832_24", "agg_sd_QID832_25", "agg_sd_QID832_26", "agg_sd_QID832_27", "agg_sd_QID832_28", "agg_sd_QID832_29", "agg_sd_QID832_9", "agg_time_spent_speaking", "agg_vge_QID867_2", "agg_vge_QID867_3", "agg_vge_QID867_4", "agg_vge_QID868_1", "agg_vge_QID868_2", "agg_vge_QID868_3", "agg_vge_QID868_4", "agg_vge_QID868_5", "agg_vge_QID868_6", "agg_vge_QID869", "agg_vge_QID870", "agg_vge_QID871", "agg_vge_QID872_1", "agg_vge_QID872_2", "agg_vge_QID872_3", "agg_vge_QID872_4", "agg_vge_QID872_5", "agg_vge_QID872_6", "agg_vge_QID872_7", "agg_vge_QID872_8", "agg_vge_QID873", "agg_vge_QID874_1", "agg_vge_QID875", "mod_lgb_Motivation_count", "mod_lgb_Motivation_per_min", "mod_lgb_Motivation_per_min_adjusted", "mod_lgb_Compensatory_Helping_count", "mod_lgb_Compensatory_Helping_per_min", "mod_lgb_Compensatory_Helping_per_min_adjusted", "mod_lgb_Contingent_Planning_count", "mod_lgb_Contingent_Planning_per_min", "mod_lgb_Contingent_Planning_per_min_adjusted", "mod_lgb_Deliberate_Planning_count", "mod_lgb_Deliberate_Planning_per_min", "mod_lgb_Deliberate_Planning_per_min_adjusted", "mod_lgb_pq_clarifying_roles_count", "mod_lgb_pq_clarifying_roles_per_min", "mod_lgb_pq_clarifying_roles_per_min_adjusted", "mod_lgb_Reactive_Planning_count", "mod_lgb_Reactive_Planning_per_min", "mod_lgb_Reactive_Planning_per_min_adjusted", "mod_lgb_Transactive_Memory_count", "mod_lgb_Transactive_Memory_per_min", "mod_lgb_Transactive_Memory_per_min_adjusted"
		]
	},
	"topic": "agent/gold"
}
```

## Message Example, msg.sub_type == "bullion"

```json
{
	"header": {"timestamp": "2022-01-20T23:37:03.946Z", "message_type": "event", "version": "1.1"}, 
	"msg": {
		"sub_type": "bullion", 
		"version": "0.1.1", 
		"source": "gallup_ta2_agent_gold", 
		"timestamp": "2022-03-10T01:12:12.111541Z", 
		"experiment_id": "[supplied outside of agent]", 
		"trial_id": "[supplied outside of agent]"}, 
	"data": {
		"gold_msg_id": "75d1af2b-0fd8-4cd4-ad73-7978bb88eebc", 
		"gold_pub_minute": 14, 
		"created_ts": "2022-03-10T01:12:12.109450Z", 
		"gold_results": [
			{
				"participant_id": "P000421", 
				"agg_trial": "20220310_010712", 
				"agg_minute": 14, 
				"agg_num_utterances": 159, "agg_avg_num_words": 8.761006289308176, "agg_time_spent_speaking": 89385393.3, "agg_first_word": 22, "agg_last_word": 15, "agg_dominant_speaker": 1.4666666666666666, "agg_long_utterance": 32, "agg_planning_all": 24.0, "agg_approval_terms": 32.0, "agg_role_terms": 9.0, "agg_entities_all": 92.0, "agg_coordination_terms": 19.0, "agg_action_terms": 396, "mod_lgb_Motivation_count": 2, "mod_lgb_Motivation_per_min": 0.14285714285714285, "mod_lgb_Motivation_per_min_adjusted": 1.7409366882791724, "mod_lgb_Compensatory_Helping_count": 3, "mod_lgb_Compensatory_Helping_per_min": 0.21428571428571427, "mod_lgb_Compensatory_Helping_per_min_adjusted": 10.667903369953516, "mod_lgb_Contingent_Planning_count": 0, "mod_lgb_Contingent_Planning_per_min": 0.0, "mod_lgb_Contingent_Planning_per_min_adjusted": -0.4120285776979025, "mod_lgb_Deliberate_Planning_count": 17, "mod_lgb_Deliberate_Planning_per_min": 1.2142857142857142, "mod_lgb_Deliberate_Planning_per_min_adjusted": 10.294411495192586, "mod_lgb_pq_clarifying_roles_count": 7, "mod_lgb_pq_clarifying_roles_per_min": 0.5, "mod_lgb_pq_clarifying_roles_per_min_adjusted": 4.505098973146806, "mod_lgb_Reactive_Planning_count": 3, "mod_lgb_Reactive_Planning_per_min": 0.21428571428571427, "mod_lgb_Reactive_Planning_per_min_adjusted": 3.1791489309954146, "mod_lgb_Transactive_Memory_count": 13, "mod_lgb_Transactive_Memory_per_min": 0.9285714285714286, "mod_lgb_Transactive_Memory_per_min_adjusted": 7.083657640922973
			}, {
				"participant_id": "P000422", 
				"agg_trial": "20220310_010712", 
				"agg_minute": 14, "agg_num_utterances": 50, "agg_avg_num_words": 7.46, "agg_time_spent_speaking": 29016215.0, "agg_first_word": 1, "agg_last_word": 1, "agg_dominant_speaker": 1.0, "agg_long_utterance": 7, "agg_planning_all": 5.0, "agg_approval_terms": 6.0, "agg_role_terms": 0.0, "agg_entities_all": 25.0, "agg_coordination_terms": 2.0, "agg_action_terms": 84, "mod_lgb_Motivation_count": 1, "mod_lgb_Motivation_per_min": 0.07142857142857142, "mod_lgb_Motivation_per_min_adjusted": 0.44283304532049045, "mod_lgb_Compensatory_Helping_count": 0, "mod_lgb_Compensatory_Helping_per_min": 0.0, "mod_lgb_Compensatory_Helping_per_min_adjusted": -0.5380102897187954, "mod_lgb_Contingent_Planning_count": 1, "mod_lgb_Contingent_Planning_per_min": 0.07142857142857142, "mod_lgb_Contingent_Planning_per_min_adjusted": 2.930094835228988, "mod_lgb_Deliberate_Planning_count": 3, "mod_lgb_Deliberate_Planning_per_min": 0.21428571428571427, "mod_lgb_Deliberate_Planning_per_min_adjusted": 0.8046398824203551, "mod_lgb_pq_clarifying_roles_count": 2, "mod_lgb_pq_clarifying_roles_per_min": 0.14285714285714285, "mod_lgb_pq_clarifying_roles_per_min_adjusted": 0.4748834805378091, "mod_lgb_Reactive_Planning_count": 0, "mod_lgb_Reactive_Planning_per_min": 0.0, "mod_lgb_Reactive_Planning_per_min_adjusted": -0.9691637927124943, "mod_lgb_Transactive_Memory_count": 0, "mod_lgb_Transactive_Memory_per_min": 0.0, "mod_lgb_Transactive_Memory_per_min_adjusted": -1.2002359632330917
			}, {
				"participant_id": "P000423", 
				"agg_trial": "20220310_010712", 
				"agg_minute": 14, 
				"agg_num_utterances": 129, "agg_avg_num_words": 7.62015503875969, "agg_time_spent_speaking": 72615827.4, "agg_first_word": 17, "agg_last_word": 18, "agg_dominant_speaker": 0.9444444444444444, "agg_long_utterance": 19, "agg_planning_all": 24.0, "agg_approval_terms": 11.0, "agg_role_terms": 3.0, "agg_entities_all": 55.0, "agg_coordination_terms": 7.0, "agg_action_terms": 245, "mod_lgb_Motivation_count": 2, "mod_lgb_Motivation_per_min": 0.14285714285714285, "mod_lgb_Motivation_per_min_adjusted": 1.7409366882791724, "mod_lgb_Compensatory_Helping_count": 3, "mod_lgb_Compensatory_Helping_per_min": 0.21428571428571427, "mod_lgb_Compensatory_Helping_per_min_adjusted": 10.667903369953516, "mod_lgb_Contingent_Planning_count": 1, "mod_lgb_Contingent_Planning_per_min": 0.07142857142857142, "mod_lgb_Contingent_Planning_per_min_adjusted": 2.930094835228988, "mod_lgb_Deliberate_Planning_count": 13, "mod_lgb_Deliberate_Planning_per_min": 0.9285714285714286, "mod_lgb_Deliberate_Planning_per_min_adjusted": 7.583048177257664, "mod_lgb_pq_clarifying_roles_count": 5, "mod_lgb_pq_clarifying_roles_per_min": 0.35714285714285715, "mod_lgb_pq_clarifying_roles_per_min_adjusted": 2.8930127761032076, "mod_lgb_Reactive_Planning_count": 4, "mod_lgb_Reactive_Planning_per_min": 0.2857142857142857, "mod_lgb_Reactive_Planning_per_min_adjusted": 4.561919838898051, "mod_lgb_Transactive_Memory_count": 7, "mod_lgb_Transactive_Memory_per_min": 0.5, "mod_lgb_Transactive_Memory_per_min_adjusted": 3.2603221313124817
			}
		]
	}, 
	"topic": "agent/gold"
}
```

Please note document draft status. Revisions are anticipated, above information and examples are not yet final and subject to change.

Revision Notes:
[2022-05-12] Updated schema to reflect current version output (0.2.0)
[2022-03-10] Updated to reflect current, schema, and further reference documentation pending status
[2022-01-31] Begin.
