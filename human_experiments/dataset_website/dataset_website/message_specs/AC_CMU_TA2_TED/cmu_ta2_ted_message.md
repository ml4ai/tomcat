# CMU TA2  - Team Effectiveness Diagnostic Analytic Component Event Message Format

This data message subtype, `AC:TED`, contains data from the CMU TA2 Team Effectiveness Diagnostic Analytic Component.

## TOPIC

agent/ac/ac_cmu_ta2_ted/ted

## Message Fields

| Field Name | Type | Description | Range |
| --- | --- | ---| ---|
| header | object | From Common Message Format section
| msg | object | From Event Header Format section 
| data.elapsed_ms | number | The number of elapsed milliseconds since mission start from the State message |
| data.delta_ms | number | Time difference from previous message | 
| data.team_score | number | Points scored in previous period |
| data.team_score_agg | number | Total score since mission start |
| data.process_effort_s | number | Player seconds spent taking action (effort) in previous period. Max = 3 players active for 10s each. | [0, 30] |
| data.process_effort_agg | number | Total effort since mission start. Aggregated and divided by total player seconds elapsed  | [0, 1] |
| data.process_skill_use_s | number | Player seconds spent exerting role-congruent effort in previous period | [0, 30] |
| data.process_skill_use_rel | number | Player seconds spent exerting role-congruent effort relative to effort exerted in previous period | [0, 1] |
| data.process_skill_use_agg | number | % time spent exerting role-congruent effort since mission start | [0, 1] |
| data.process_workload_burnt | number | % search and rescue workload executed in previous period | [0, 1] |
| data.process_workload_burnt_agg | number | % search and rescue workload executed since mission start | [0, 1] |
| data.comms_total_words | number | Total words spoken by the team since mission start |
| data.comms_equity | number | Balance of conversation (std dev. in words spoken across players) since mission start  |
| data.action_explore_s | number | Player seconds spent searching unexplored cells in previous period | [0, 30] |
| data.explore_count | number | New cells explored in previous period | [0, 9000] max assumed based on study 2 map (is configurable) |
| data.process_coverage | number | New cells explored in previous period | [0, 9000] max assumed based on study 2 map (is configurable) |
| data.process_coverage_agg | number | % cells explored since mission start | [0, 1] |
| data.triage_count | number | New victims triaged in previous period |
| data.action_triage_s | number | Player seconds spent triaging victims in previous period | [0, 30] |
| data.process_triaging_agg | number | % of maximum possible triage time completed since mission start. Max triage time is 450s [50 (regular) * 7.5 (s) + 5 (critical) * 15 (s)]. | [0, 1] 
| data.dig_rubble_count | number | New rubble destroyed in previous period |
| data.action_dig_rubble_s | number | Player seconds spent destroying rubble in previous period | [0, 30] |
| data.move_victim_count | number | Number of victims moved in previous period |
| data.action_move_victim_s | number | Player seconds spent moving victims in previous period | [0, 30] |
| data.inaction_stand_s | number | Player seconds spent not engaged in any activity in previous period | [0, 30] |

## Message Example

```json
{
  "header": {
    "timestamp": "2020-08-25T01:56:21.517415Z",
    "version": "1.1",
    "message_type": "agent"
  },
  "msg": {
    "sub_type": "AC:TED",
    "timestamp": "2020-08-22T04:06:18.846Z",
    "version": "0.1",
    "source": "ac_cmu_ta2_ted",
    "experiment_id": "c6f930a2-357b-4c24-9c4e-42b1c8d9458f",
    "trial_id": "25edd30a-2a4b-4229-9237-1304ea8cc439",
    "replay_id": "a44120f7-b5ba-47a3-8a21-0c76ede62f75"
  },
  "data": {
    "elapsed_ms": 894265,
    "delta_ms": 10034,
    "process_coverage": 23,
    "process_coverage_agg": 0.6776,
    "inaction_stand_s": 6.605,
    "action_triage_s": 0.0,
    "triage_count": 0,
    "action_dig_rubble_s": 0.851,
    "dig_rubble_count": 1,
    "action_move_victim_s": 10.034,
    "move_victim_count": 0,
    "action_explore_s": 5.557,
    "explore_count": 23,
    "process_triaging_agg": 0.582,
    "team_score": 0,
    "team_score_agg": 580,
    "comms_total_words": 15,
    "comms_equity": 8.6603,
    "process_skill_use_s": 16.442,
    "process_effort_s": 23.497,
    "process_skill_use_rel": 0.6997,
    "process_workload_burnt": 0.0013,
    "process_skill_use_agg": 0.516,
    "process_effort_agg": 0.822,
    "process_workload_burnt_agg": 0.6298
  }
}
```
