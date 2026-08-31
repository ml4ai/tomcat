# Data Subtype: Event:cognitive_load Message Measure Format
This data message subtype, is used to communicate measure value and confidence information.

## TOPIC

agent/measure/<agent_name>/load

## Message Fields

| Field Name | Type    | Description              |
|------------|---------|--------------------------|
| value      | number  | Measure value            |
| confidence | number  | Measure Confidence value |

## Examples

```json
{
    "value": 4.654498457656448,
    "confidence": 0.5052246336163365
}
```