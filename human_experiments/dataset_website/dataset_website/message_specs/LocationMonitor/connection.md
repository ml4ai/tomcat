# Data Subtype: Event:location Message connection Format
This data message subtype, is used to communicate connection's id and it's connected locations.  This is the object structure of elements in the connections and exited_connections fields of the location event message.

## TOPIC

observations/events/player/location

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| id | string | The id of the connection
| connected_locations | List of string | (optional) The ids of the locations this connection connects 

## Message Example

```json
{
  "id": "ce_112_77_112_79",
  "connected_locations": [
    "lh_1",
    "lh_2_1"
  ]
}
```