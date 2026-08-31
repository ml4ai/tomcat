# Common Message Header
All messages will have a common header to describe the basic characteristics of the follow message.
## Message Fields
| Field Name | Type | Description |
--- | --- | ---
| timestamp | string | The time the message was published. UTC timezone formatted as ISO 8601: YYYY-MM-DDThh:mm:ss.ssssZ |
| message_type | string | One of the defined message types |
| version | string | The version of the message type object |


## Message Example
```json
{
"timestamp": "2019-12-26T12:47:23.1234Z",
"message_type": "observation",
"version": "1.1"
}
```

## Version Change History
VERSION | DATE | DETAILS
| --- | --- | --- |  
| 1.1 | 1/12/2021 | Added "experiment" to message_type enum |
| 1.0 | NA | Initial Version |

