# GroundTruth Subtype:Mission:VictimList Victim Message Format
This data message subtype is used to communicate victim position information. This is the object structure of elements in the mission_victim_list field of the victimlist_groundtruth_message.

## TOPIC

ground_truth/mission/victims_list

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| x | int | The current x location of the victim block
| y | int  | The current y location of the victim block
| z | int  | The current z location of the victim block
| block_type | string | The victim block type
| room_name | string | The room name the victim block is in
| unique_id | int | The unique_id of the victim

## Message Example

```json
{
	"x" : -2088,
	"y" : 60,
	"z" : 146,
	"block_type" : "block_victim_1",
	"room_name" : "Security Office",
	"unique_id" : 33	
}

```

## CHANGE HISTORY

VERSION | DATE | DETAILS

0.6 | 4/27/2021 | Included a victim_id for each victim. Changed all numbers to integers

0.5 | NA | Initial Creation
