# Data Subtype: Event Message Format
This data message subtype is used to communicate event data for both the world and the player. 

## TOPIC

observations/events/player/tool_used

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section
| msg | object | From the Common Event Message Format section 
| data.mission_timer | string | the mission time of the event
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start
| data.participant_id | string | the participant id for the player being reported
| data.playername | string | [Deprecated]The name of the entity that changed state
| data.tool_type | string | The type of tool being used. From ENUM --> [ "Shovel", "Med_Kit" ]
| data.durability | number | The number of uses (hits) left in the tool
| data.count | number | The number of tools of this type in the inventory
| data.target_block_x | number | The X Position of the block being hit
| data.target_block_y | number | The Y Position of the block being hit
| data.target_block_z | number | The Z Position of the block being hit
| data.target_block_type | string | A string representation of a blocktype defined by Minecraft such as Gravel, Clay, Air, or a blocktype defined by the AdaptMod such as Victim Block 1 (Green), Victim Block 2 (Yellow) 


## Message Example

```json
{
"header": {
	"timestamp": "2019-12-26T12:47:23.1234Z",
	"message_type": "event",
	"version": "1.0"
	},
"msg": {
	"experiment_id": "123e4567-e89b-12d3-a456-426655440000", 
    "trial_id": "123e4567-e89b-12d3-a456-426655440000",
	"timestamp": "2019-12-26T14:05:02.1412Z",
	"source": "simulator",
	"sub_type": "Event:ToolUsed",
	"version": "1.1"},
"data": {
	"mission_timer":"8 : 36",
	"elapsed_milliseconds": 15113,
	"participant_id": "E000321",	
	"tool_type": "MEDKIT",
	"durability": 6,
	"count": 1,
	"target_block_x": -2180,
	"target_block_y": 64,
	"target_block_z": 128,
	"target_block_type": "minecraft:gravel"
	}
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS
| --- | --- | --- |
1.1 | 8/13/2021 | deprecated playername and added participant_id
1.0 | 3/5/2021 | Standardized tool_type enum field to Upper Case tools for all 3 roles. Changed target block type to use registry name instead of localized name, changed count to be a number instead of a string
0.5 | 1/26/2021 | Initial state when ported from ADAPT project