# Data Subtype: Event:location Message location Format
This data message subtype, is used to communicate location ids and names.  This is the object structure of element sin the locations and exited_locations field of the location event message.

## TOPIC

observations/events/player/location

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| id | string | The id of the location
| name | string | (optional) The name of the location 

## Message Example

```json
{
  "id": "lh",
  "name": "Left Hallway"
}
```