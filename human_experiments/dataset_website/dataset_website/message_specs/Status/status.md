# Status Message Format
A status message describes the status of a component in the testbed.  These messages can be used as a simple heartbeat to indicate that the component is still functioning, or can provide more descriptive messages such as errors and failure indicators.

## TOPIC

### ALL STATUS
status/+/+

### HEARTBEATS

status/+/heartbeats


## Message Fields

| Field Name | Type | Description|
 --- | --- | ---
| header | object | From Common Header Format section
| msg | object | From Common Message format section. NOTE: the experiment_id, trial_id, and replay fields need not be filled in for heartbeat messages because the expeiment/trial may not have begin
| data.state | string | The basic state of the component. One of ["ok", “info”, “warn”, “error”, “fail”]
| data.status | string | [Optional] A message giving more detail about the status of the component
| data.active | boolean | [Optional] true or false as to if the component is actively functioning

## Message Examples
```json
{ "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "status",
    "version": "0.1"
    },
"msg": {
    "timestamp": "2019-12-26T14:05:02.5443Z",
    "sub_type": "heartbeat",
    "source": "reference_agent",
    "version": "0.3"
    },
"data": {
    "state": "ok"
    }
}
```

```json
{ "header": {
    "timestamp": "2019-12-26T14:05:02.3412Z",
    "message_type": "status",
    "version": "0.1"
    },
"msg": {
    "timestamp": "2019-12-26T14:05:02.5443Z",
    "sub_type": "heartbeat",
    "source": "malmo",
    "version": "0.3"
    },
"data": {
    "active": false,
    "state": "error",
    "status": "Illegal format message received"
    }
}
```