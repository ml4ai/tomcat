# Data Subtype: Event:dyad Message Format
This data message subtype, `Event:dyad`, is used to communicate player dyadic events.

## TOPIC

observations/events/player/dyad

## Message Fields

| Field Name | Type | Description
| --- | --- | ---|
| header | object | From Common Message Format section |
| msg | object | From the event Header Format section |
| data.id | string | Unique id for this dyad. |
| data.type | string | dyad message type ENUM of ["start", "update", "end"] |
| data.elapsed_milliseconds | number | the number of elapsed milliseconds since mission start |
| data.participants | List of Dyad_Participant | list of participants in this dyad |
| data.in-dyad-probability | number | The probability that these participants are in a dyad 0.0 - 1.0 |
| data.duration | number | published when the type is 'end' and indicates how long the dyad lasted in milliseconds. |

## Message Example

```json
{
    "header": {
        "timestamp": "2021-10-11T04:09:04.367033Z",
        "message_type": "event",
        "version": "1.1"
    },
    "msg": {
        "sub_type": "Event:dyad",
        "version": "1.0",
        "source": "IHMC_Dyad_AC",
        "timestamp": "2021-10-11T04:09:04.367033Z",
        "experiment_id": "39decb46-72fc-4a54-9a78-7571b6011e27",
        "trial_id": "1d835d89-8fce-4e28-b3f0-549dac423d25"
    },
    "data": {
        "id": "7b87ee9f-fbe7-448d-b00f-35b93844a623",
        "event": "start",
        "elapsed_milliseconds": 30615,
        "participants": [
            { "callsign": "red", "participant_id": "p1", "role": "Search_Specialist" },
            { "callsign": "green", "participant_id": "p2", "role": "None" }
        ],
        "in-dyad-probability": 0.5
    }
}
```
```json
{
    "header": {
        "timestamp": "2021-10-11T04:11:16.755022Z",
        "message_type": "event",
        "version": "1.1"
    },
    "msg": {
        "sub_type": "Event:dyad",
        "version": "1.0",
        "source": "IHMC_Dyad_AC",
        "timestamp": "2021-10-11T04:11:16.755022Z",
        "experiment_id": "39decb46-72fc-4a54-9a78-7571b6011e27",
        "trial_id": "1d835d89-8fce-4e28-b3f0-549dac423d25"
    },
    "data": {
        "id": "7b87ee9f-fbe7-448d-b00f-35b93844a623",
        "event": "update",
        "elapsed_milliseconds": 163003,
        "participants": [
            { "callsign": "red", "participant_id": "p1", "role": "Search_Specialist" },
            { "callsign": "green", "participant_id": "p2", "role": "None" }
        ],
        "in-dyad-probability": 1.0
    }
}
```
```json
{
    "header": {
        "timestamp": "2021-10-11T04:11:27.917094Z",
        "message_type": "event",
        "version": "1.1"
    },
    "msg": {
        "sub_type": "Event:dyad",
        "version": "1.0",
        "source": "IHMC_Dyad_AC",
        "timestamp": "2021-10-11T04:11:27.917094Z",
        "experiment_id": "39decb46-72fc-4a54-9a78-7571b6011e27",
        "trial_id": "1d835d89-8fce-4e28-b3f0-549dac423d25"
    },
    "data": {
        "id": "7b87ee9f-fbe7-448d-b00f-35b93844a623",
        "event": "end",
        "elapsed_milliseconds": 174166,
        "participants": [
            { "callsign": "red", "participant_id": "p1", "role": "Search_Specialist" },
            { "callsign": "green", "participant_id": "p2", "role": "None" }
        ],
        "in-dyad-probability": 0.0,
        "duration": 143551
    }
}
```