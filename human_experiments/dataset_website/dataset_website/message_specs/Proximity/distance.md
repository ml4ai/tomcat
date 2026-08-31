# Data Subtype: Event:proximity Message location Format
This data message subtype, is used to communicate distance information.

## TOPIC

observations/events/player/proximity

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| id | string | The id of the object whose distance is being reported.  Can be players, locations, connections... |
| distance | number | Distance (in Minecraft blocks) to the specified object id |

## Examples

```json
{ "id": "blue", "distance": 132.9 }
```

```json
{ "id": "r110", "distance": 97.2 }
```