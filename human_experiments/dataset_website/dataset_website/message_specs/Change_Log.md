|MESSAGE | VERSION | DATE | DETAILS|
| ----------------------------| ---------------------------| --------------------------| ------------------------------------------------------- |
| Event:VictimEvacuated  2.2 | 04/29/2022 | Added [victim_a,victim_b] to possible types of victims evacuated
| Event:RubbleCollapse | 2.1 | 4/13/2021 | Topic change observations/events/server/rubble_collapse --> observations/events/player/rubble_collapse |
| Event:PuzzleTextSummary | 2.0 | 03/10/22 | Initial Spec Created
| Mission:RoleText | 2.1 | 3/10/2022 | topic corrected to ground_truth from groundtruth
| Event:PlanningStage | 2.0 | 3/04/2022 | Initial Schema created |
| Event:PerturbationRubbleLocations | 2.1 | 3/04/2022 | Added Timing information |
| Event:ProximityBlockInteraction | 2.1 | 3/04/2022 | Added "awake" key to data section. Will be true when the victim wakes up due to 1 medic and + n required teammates present |
| observation:asr | 0.1 | 2/14/2022 | Removed a number of unused/redunded fields from ASR messages. Intermediate transcriptions re-enabled and additional fields added |
| agent:Intervention:Chat | .6 | 1/25/2022 | fixed error in documentation.  The source field should be in the msg section and not the data section.|
| Event:Perturbation | 2.2 | 1/24/2021 | Updated the Message Fields section "state" key to "mission_state" in this file. Also made mission_state a required key as opposed to state |
| Event:TrainingTask | 2.0 | 1/17/2022 | Initial Schema Creation |
| Event:CompetencyTask | 2.0 | 1/17/2022 | Schema updated to differentiate between training and competency tasks - similar schema exists for training tasks under TrainingTask folder. Added various enums for the task messages as well as standard player id fields and timestamps. |
| Event:PerturbationRubbleLocations | 2.0 | 12/23/2021 | Initial spec created |
| Event:Perturbation | 2.1 | 12/23/2021 | Added enum type [rubble] to type field |
| Event:Signal | 2.1 | 12/21/2021 | updated message enums to [No Victim Detected, Regular Victim Detected, Critical Victim Detected] |
| Event:ProximityBlockInteraction | 2.0 | 12/20/2021 | Changed Event:ProximityVictimInteraction to Event:ProximityBlockInteraction and topic to "observations/events/player/proximity_block" |
| Mission:RoleText | 2.0 | 12/3/21 | Initial Spec created
| Event:Perturbation | 2.0 | 11/30/2021 | Initial spec created |
| Event:Signal | 2.0 | 11/23/2021 | updated message from Beep to VictimSignal with new properties -> roomname, participant_id |
| Event:MarkerRemoved | 2.1 | 11/19/2021 | updated marker block enums --> removed [critical,wildcard] replaced with [novictim,sos] for red,green,blue variations |
| Event:MarkerPlaced | 2.1 | 11/19/2021 | updated marker block enums --> removed [critical,wildcard] replaced with [novictim,sos] for red,green,blue variations |
| Event:VictimEvacuated | 2.1 | 11/11/2021 | changed victim_safe_a/b/c to victim_saved_a/b/c as in the mod [victim_saved_a,victim_saved_b,victim_saved_c] |
| Event:VictimPlaced | 2.1 | 11/11/2021 | changed victim_safe_a/b/c to victim_saved_a/b/c as in the mod [victim_a,victim_b,victim_c,victim_saved_a,victim_saved_b,victim_saved_c] |
| Event:VictimPickedUp | 2.1 | 11/11/2021 | changed victim_safe_a/b/c to victim_saved_a/b/c as in the mod [victim_a,victim_b,victim_c,victim_saved_a,victim_saved_b,victim_saved_c] |
| rollcall:request/response | 0.1 | 11/1/2021 | Initial Spec Created |
| Event:RoleSelected | 2.0 | 11/1/2021 | change role type enums in data.new_role and data.prev_role key to [None,Admin,Transport_Specialist,Engineering_Specialist,Medical_Specialist] |
| Event:RubbleCollapse | 2.0 | 11/1/2021 | Initial schema creation |
| Event:Triage | 2.0 | 11/1/2021 | change victim type identifier in data.type key to [victim_a,victim_b,victim_c] |
| Event:VictimPlaced | 2.0 | 11/1/2021 | change victim type identifier in data.type key to [victim_a,victim_b,victim_c,victim_safe_a,victim_safe_b,victim_safe_c] |
| Event:VictimPickedUp | 2.0 | 11/1/2021 | change victim type identifier in data.type key to [victim_a,victim_b,victim_c,victim_safe_a,victim_safe_b,victim_safe_c] |
| Event:VictimEvacuated | 2.0 | 11/1/2021 | Initial schema creation |
| Event:MarkerRemoved | 2.0 | 11/1/2021 | updated marker block enums to the following list:[red_abrasion,red_bonedamage,red_critical,red_regularvictim,red_criticalvictim,red_rubble,red_threat,red_wildcard,green_abrasion,green_bonedamage,green_critical,green_regularvictim,green_criticalvictim,green_rubble,green_threat,green_wildcard,blue_abrasion,blue_bonedamage,blue_critical,blue_regularvictim,blue_criticalvictim,blue_rubble,blue_threat,blue_wildcard] |
| Event:MarkerPlaced | 2.0 | 11/1/2021 | updated marker block enums to the following list:[red_abrasion,red_bonedamage,red_critical,red_regularvictim,red_criticalvictim,red_rubble,red_threat,red_wildcard,green_abrasion,green_bonedamage,green_critical,green_regularvictim,green_criticalvictim,green_rubble,green_threat,green_wildcard,blue_abrasion,blue_bonedamage,blue_critical,blue_regularvictim,blue_criticalvictim,blue_rubble,blue_threat,blue_wildcard] |
| Event:VictimPlaced | 1.3 | 8/13/2021 | deprecated playername and added participant_id |
| Event:VictimPickedUp | 1.3 | 8/13/2021 | Deprecated playername and added participant_id |
| state | 1.1 | 8/13/2021 | deprecated playername and added participant_id
| Event:Triage | 1.3 | 8/13/2021 | deprecated playername and added participant_id |
| Event:ToolUsed | 1.1 | 8/13/2021 | deprecated playername and added participant_id |
| Event:ToolDepleted | 1.1 | 8/13/2021 | deprecated playername and added participant_id |
| Event:RubbleDestroyed | 1.1 | 8/13/2021 | deprecated playername and added participant_id |
| Event:Door | 1.2 | 8/12/2021 | added participant_id and made playername optional and deprecated |
| Event:ProximityBlockInteraction | 1.2 | 8/12/2021 | deprecated playername and added participant_id |
| Event:VictimPlaced | 1.2 | 4/27/2021 | Added victim_id field |
| Event:VictimPickedUp | 1.2 | 4/27/2021 | Added victim_id field |
| Event:Triage | 1.2 | 4/27/2021 | Added victim_id field |
| Event:ProximityBlockInteraction | 1.1 | 4/27/2021 | Added victim_id field |
| Mission:VictimList | 0.6 | 4/27/2021 | Included a victim_id for each victim. Changed all numbers to integers |
| Event:VictimPlaced | 1.1 | 4/12/2021 | data.color changed to data.type with types [REGULAR,CRITICAL] |
| Event:VictimPickedUp | 1.1 | 4/12/2021 | data.color changed to data.type with types [REGULAR,CRITICAL] |
| Event:Triage | 1.1 | 4/12/2021 | data.color changed to data.type with types [REGULAR,CRITICAL] |
| Common Message | 0.6 | 4/12/2021 | Added "replay_parent_type", "replay_parent_id" | 
| Event:VictimPlaced | 1.0 | 3/10/2021 | data.color is now upper case with only GREEN and YELLOW as options |
| Event:VictimPickedUp | 1.0 | 3/10/2021 | data.color is now upper case with only GREEN and YELLOW as options |
| Event:Triage | 1.0 | 3/10/2021 | data.color is now upper case with only GREEN and YELLOW as options |
| Event:MarkerRemoved | 1.0 | 3/10/2021 | Initial Schema creation |
| Event:MarkerPlaced | 1.0 | 3/10/2021 | Initial Schema creation |
| Event:RoleSelected | 1.0 | 3/10/2021 | Initital creation of schema |
| Event:ToolUsed | 1.0 | 3/5/2021 | Standardized tool_type enum field to Upper Case tools for all 3 roles. Changed target block type to use registry name instead of localized name, changed count to be a number instead of a string |
| Event:ToolDepleted | 1.0 | 3/5/2021 | Standardized tool_type enum field to Upper Case tools for all 3 roles. |
| Event:ProximityBlockInteraction | 1.0 | 3/1/2021 | Initial Creation |
| Event:Chat | 1.0 | 3/1/2021 | Added elapsed_milliseconds field |
| Event:Door | 1.1 | 3/1/2021 | Added elapsed_milliseconds field |
| Common Message | 0.5 | 3/1/2021 | Version tracking begins |
| Intervention:Chat | 0.5 | 3/1/2021 | Ported from Adapt |
| Event:ToolUsed | 0.5 | 1/26/2021 | Initial state when ported from ADAPT project |
| Event:ToolDepleted | 0.5 | 1/26/2021 | Initial state when ported from ADAPT project |
| Common Header | 1.1 | 1/12/2021 | Added "experiment" to message_type enum |
| Event:Door | 1.0 | 6/30/2020 | Initial version |



















