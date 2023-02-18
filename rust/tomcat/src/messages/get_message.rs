use paho_mqtt as mqtt;
use serde::Deserialize;
use serde_json as json;

/// Deserialize message to a struct.
pub fn get_message<'a, T: Deserialize<'a>>(message: &'a mqtt::Message) -> T {
    json::from_slice::<T>(message.payload()).unwrap_or_else(|_| {
        let v: json::Value = json::from_str(&message.payload_str()).unwrap();
        panic!(
            "Unable to deserialize JSON payload {} for message arriving on topic '{}'",
            json::to_string_pretty(&v).unwrap(),
            message.topic(),
        )
    })
}

/// Deserialize message to a struct.
pub fn get_message_from_string<'a, T: Deserialize<'a>>(message: &'a str, topic: &str) -> T {
    json::from_str::<T>(message).unwrap_or_else(|_| {
        let v: json::Value = json::from_str(message).unwrap();
        panic!(
            "Unable to deserialize JSON payload {} for message arriving on topic '{}'",
            json::to_string_pretty(&v).unwrap(),
            topic,
        )
    })
}
