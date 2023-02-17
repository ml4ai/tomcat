use iso8601_timestamp::Timestamp;
use serde::{Deserialize, Serialize};

#[allow(non_camel_case_types)]
#[derive(Debug, Serialize, Deserialize)]
pub enum MessageType {
    control,
    simulator_event,
    status,
    metadata,
    agent,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Header {
    pub timestamp: Timestamp,
    pub version: String,
    pub message_type: MessageType,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Msg {
    pub trial_id: String,
    pub experiment_id: String,
    pub timestamp: Timestamp,
    pub source: String,
    pub version: String,
    pub sub_type: String,
    pub replay_parent_id: Option<String>,
    pub replay_id: Option<String>,
    pub replay_parent_type: Option<String>,
}
