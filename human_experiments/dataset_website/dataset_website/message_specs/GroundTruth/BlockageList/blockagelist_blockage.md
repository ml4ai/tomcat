# GroundTruth Subtype:Mission:BlockageList Blockage Message Format
This data message subtype is used to communicate blockage position information. This is the object structure of elements in the mission_blockage_list field of the blockagelist_groundtruth_message.

## TOPIC

ground_truth/mission/blockages_list

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| x | float | The current x location of the blockage block
| y | float | The current y location of the blockage block
| z | float | The current z location of the blockage block
| block_type | string | The blockage block type
| room_name | string | The room name the blockage block is in
| feature_type | string | The type of map feature this block is associated with = Blockage

## Message Example

```json
{
	"x" : -2088.0,
	"y" : 60.0,
	"z" : 146.0,
	"block_type" : "cobblestone",
	"room_name" : "Security Office",
	"feature_type" : "Security Office"	
}

```