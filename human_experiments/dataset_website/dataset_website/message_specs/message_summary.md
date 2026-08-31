# Message Summary
This is a summary list of all of the messages published on the testbed message bus.

| Message type | message subtype | publisher | topic | when triggered | content description | Detail Documentation | notes
| --- | --- | --- | --- | --- | --- | --- | --- |
| chat | Event:chat | simulator | minecraft/chat | player chats | chat sender, addressees and text | [Doc](Chat/chat_event_message.md) |
| event | Event:door | simulator | observations/events/player/door | When door opens or closes | door state and location | [Doc](Door/door_event_message.md) |
| event | Event:ItemDrop | simulator | observations/events/player/itemdrop | when player drops item | player, item, location | [Doc](Item/itemdropped_event_message.md)
| event | Event:ItemEquipped | simulator | observations/events/player/itemequipped | when player equips item | player, item | [Doc](Item/itemequipped_event_message.md)
| event | Event:ItemPickup | simulator | observation/events/player/itempickup | when player picks up item | player, item, location | [Doc](Item/itempickup_event_message.md)
| event | Event:ItemUsed | simulator | observations/events/player/itemuse | when player uses item | player, item, location | [Doc](Item/itemused_event_message.md)
| event | Event:Lever | simulator | observations/events/player/lever | a lever is flipped | player, powered, location | [Doc](Lever/lever_event_message.md)
| event | Event:PlayerJumped | simulator | observations/events/player/jumped | player jumps | player, location | [Doc](PlayerEvent/jump_event_message.md)
| event | Event:PlayerSprinting | simulator | observations/events/player/sprinting | player sprints | player, sprint state | [Doc](PlayerEvent/sprint_event_message.md)
| event | Event:Scoreboard | simulator | observations/events/scoreboard | scoreboard changes | mission time, player, score | [Doc](Scoreboard/scoreboard_event_message.md)
| event | Event:Triage | simulator | observations/events/player/triage | Interaction with victim | player, triage state, victim location | [Doc](Triage/triage_event_message.md) |
| event | Event:RoleSelected | simulator | observations/events/player/role_selected | A player selected a role from [Engineering_Specialist,Medical_Specialist,Transport_Specialist] | playername, mission_timer, new_role, prev_role | [Doc](RoleSelected/role_selected_event_message.md) 
| event | Event:ProximityBlockInteraction | simulator | observations/events/player/proximity_block | A player has interacted with a proximity block | playername, mission_timer, action_type, players_in_range, position, elapsed-milliseconds | [Doc](ProximityBlockInteraction/proximity_block_interaction_event_message.md) 
| event | Event:PlayerFrozenStateChange | simulator | observations/events/player/freeze | A player has either frozen or unfrozen | playername, mission_timer, state_changed_to, medic_playername, position, elapsed-milliseconds | [Doc](PlayerFrozenStateChange/player_frozen_state_change_event_message.md) 
| event | Event:ToolUsed | simulator | observations/events/player/tool_used | A tool was used with Right Click [Shovel,Med-Kit] | playername, mission_timer, tool_type,durability,count,target_block_x,target_block_y,target_block_z,target_block_type | [Doc](ToolUsed/tool_used_event_message.md)
| event | Event:CompetencyTask | simulator | observations/events/competency/task | A competency task has been completed | playername, mission_timer, task_message | [Doc](CompetencyTask/competencytask_event_message.md)
| event | Event:TrainingTask | simulator | observations/events/training/task | A training task has been completed | playername, mission_timer, task_message | [Doc](TrainingTask/trainingtask_event_message.md)
| event | Event:ToolDepleted | simulator | observations/events/player/tool_depleted | A tool has run out of durability | playername, mission_timer, tool_type| [Doc](ToolDepleted/tool_depleted_event_message.md)
| event | Event:MarkerPlaced | simulator | observations/events/player/marker_placed | A marker has been placed by a player | playername, mission_timer, marker_x, marker_y, marker_z,color| [Doc](MarkerPlaced/marker_placed_event_message.md)
| event | Event:MarkerRemoved | simulator | observations/events/player/marker_removed | A marker has been removed by a player | playername, mission_timer, marker_x, marker_y, marker_z,color| [Doc](MarkerRemoved/marker_removed_event_message.md)
| event | Event:RubbleCollapse | simulator | observations/events/player/rubble_collapse | A rubble collapse trigger has been activated and rubble has collapsed in a predefined section of the map | mission_timer, triggerLocation xyz, rubble locationxyz| [Doc](RubbleCollapse/rubble_collapse_event_message.md)
| event | Event:VictimEvacuated | simulator | observations/events/player/victim_evacuated | A victim has been triaged and evacuated to the scoreing area| mission_timer, victim x,y,z , type, correct_area| [Doc](VictimEvacuated/victim_evacuated_event_message.md)
| event | Event:VictimPickedUp | simulator | observations/events/player/victim_picked_up | A victim has been picked up by a player | playername, mission_timer, victim_x, victim_y, victim_z,color| [Doc](VictimPickedUp/victim_picked_up_event_message.md)
| event | Event:VictimPlaced | simulator | observations/events/player/victim_placed | A victim has been placed on the ground by a player | playername, mission_timer, victim_x, victim_y, victim_z,color| [Doc](VictimPlaced/victim_placed_event_message.md) 
| event | Event:RubbleDestroyed | simulator | observations/events/player/rubble_destroyed | Rubble has been destroyed by a player | mission_timer, playername, rubble_x, rubble_y, rubble_z| [Doc](RubbleDestroyed/rubble_destroyed_event_message.md)
| event | Event:Signal | simulator | observations/events/player/signal | device senses near victim | mission time, location | [Doc](VictimSignal/victim_signal_event_message.md)
| event | Event:Pause | simulator| observations/events/pause | on pause on/off | mission timer, pause state | [Doc](Pause/pause_event_message.md)
| event | Event:MissionState | simulator | observations/events/mission | mission change in state | mission name, mission_state (e.g. start)  | [Doc](MissionState/missionstate_event_message.md)
| event | Event:Perturbation | simulator | observations/events/mission/perturbation | perturbation type and change in state |  (e.g. start)  | [Doc](Perturbation/perturbation_event_message.md)
| event | Event:PlanningStage | simulator | observations/events/mission/planning | Planning Stage start and stop |  (e.g. start)  | [Doc](PlanningStage/planningstage_event_message.md)
| event | Event:PerturbationRubbleLocations | simulator | observations/events/mission/perturbation_rubble_locations | on perturbation | rubble locations |  (e.g. xyz, type)  | [Doc](Perturbation/RubbleLocations/perturbation_rubble_locations_event_message.md)
| event | Event:location | IHMC Location Monitor agent | observations/events/player/location | when player changes rooms | player, enter room, exit room | [Doc](LocationMonitor/location_event_message.md)
| event | Event:proximity | IHMC Proximity AC agent | observations/events/player/proximity | player proximity at a specified rate | list of participant proximity info | [Doc](Proximity/proximity_event_message.md)
| event | Event:dyad | IHMC Dyad AC agent | observations/events/player/dyad | when player dyads change | dyad type, participants, duration | [Doc](Proximity/dyad_event_message.md)
| intervention | Intervention:Text | agent | agent/intervention/+/chat | when agent sends chat intervention | receiver, content, startTime | [Doc](Agent/agent_intervention_message.md)
| intervention | Intervention:Block | agent | agent/intervention/+/block | when agent sends block intervention | receiver, content, startTime | [Doc](Agent/agent_intervention_message.md)
| intervention | Intervention:Text | agent | agent/intervention/+/map| when agent sends map intervention | receiver, content, startTime | [Doc](Agent/agent_intervention_message.md)
| prediction | Prediction:State \| Prediction:Action | agent | agent/prediction/+ | when agent sends a prediction | subject, object, state, probability, duration | [Doc](Agent/Prediction/agent_prediction_message.md)
| groundtruth | Event:VictimsExpired | simulator | ground_truth/mission/victims_expired | when critical victims expire | mission timer, message | [Doc](GroundTruth/VictimsExpired/victimsexpired_event_message.md)
| groundtruth | Mission:VictimList | simulator | ground_truth/mission/victims_list | at trial start | List of victims, location, type, room name | [Doc](GroundTruth/VictimList/victimlist_groundtruth_message.json)
| groundtruth | Mission:RoleText | simulator | ground_truth/mission/role_text | at mission start | Mission Name and text for each role | [Doc](GroundTruth/RoleText/role_text_message.json)
| groundtruth | Mission:FreezeBlockList | simulator | ground_truth/mission/freezeblock_list | at trial start | List of freezeblocks, location, type, room name | [Doc](GroundTruth/FreezeBlockList/freezeblock_groundtruth_message.json)
| groundtruth | Mission:ThreatSignList | simulator | ground_truth/mission/threatsign_list | at trial start | List of threatsigns, location, type, room name | [Doc](GroundTruth/ThreatSignList/threatsign_groundtruth_message.json)
| event | Event:PuzzleTextSummary | simulator | observations/events/mission/puzzle_summary | at mission start | Mission Name and text summary for each role | [Doc](PuzzleTextSummary/puzzle_text_summary_message.json)
| observation | state | simulator | observations/state | every 100ms by default | player name, type, location, motion, yaw, pitch, life | [Doc](PlayerState/observation_state.md)
| observation | fov | FoV Tool | observations/state/player/fov | as fast as player observservations can be process | what blocks are in the player'd field of view | [Doc](PyGLFoVAgent/fov.md) | there may be several  FoV tools that will publish on subtopics.
| observation | asr | ASR Agent | agent/asr/intermediate agent/asr/final | Whenever players are speaking | Transcriptions of the players' utterances | [Doc](AC_UAZ_TA1_ASR_Agent/asr_message.md)
| observation | speech_analyzer | Speech Analyzer Agent | agent/speech_analyzer/sentiment agent/speech_analyzer/personality | When an agent/asr/final message is published | Sentiment and Personality labels based off of the utterance and extracted vocalic features | [Doc](AC_UAZ_TA1_SpeechAnalyzer/sentiment_message.md)
| event | Event:dialogue_event | UAZ Dialog Agent | agent/dialog | Whenever a 'final' ASR transcription or a Event:chat message is published to the bus| Information extracted from the utterance or chat | [Doc](DialogAgent/chat_analysis_message.md)
| experiment | | testbed controller | experiment | experiment created | Name, date, mission, exp id | [Doc](Experiment/experiment.md)
| event | Event:TrialState | testbed controller | observations/events/trial | trial or replay start | Name, date, experimenter, subjects, exp_id, trial id | [Doc](Trial/trial.md)
| control | | testbed controller | control | various testbed activities | commands of: init, start, stop, replay | [Doc](Control/Control.md)
| status | | various components | status | periodically or on state change | component name, state, status | [Doc](Status/Status.md)
| status | Status:UserSpeech | AssistDataIngester | status/asistdataingester/userspeech | After sppech captured and tranlated to text | Playername, text | [Doc](/Status/userspeech.md)
| trial | start, stop | testbed controller | trial | When a trial starts/stops | Trial information ie Subjects,Experimenter, ID's | [Doc](Trial/Trial.md)| 

# Message Sequences
This section describes important message sequences

## Experiment and Trial startup and stop
There are a sequence of messages that are published when experiments are created,
when trials are created and when trails are started and stopped.  This section
describes those sequences.

| Activity | User Action | Message type | Message subtype | Topic | Notes  
| --- | --- | --- | --- | --- | --- |
| Create experiment | Hit Create Experiment button in Control GUI | experiment | NA | experiment | Experiment_id created by metadata container 
| Create trial | Hit Create Trial button in Control GUI | trial | NA | trial | [NOT CURRENTY USED] Contains experiment_id and new trial_id plus other trial info
| Start trial | Hit Start Trial button in Control GUI | trial | NA | trial | Contains experiment_id, trial_id and trial details of trial to be started
| Game play | Game play | Various: observation, event | Various | various: observations/* | observation and event message of game play
| Start mission | Player hits start button in game | event | Event:MissionState | observations/events/mission | Start of subject playing the mission. Contains Mission name
| End trial | Hit Stop Trial button in Control GUI | trial | NA | trial | Contains experiment_id and trial_id being stopped

