use iso8601_timestamp::Timestamp;
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Header {
    pub timestamp: Timestamp,
    pub version: String,
    pub message_type: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Msg {
    pub experiment_id: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub replay_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub replay_parent_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub replay_parent_type: Option<String>,
    pub source: String,
    pub sub_type: String,
    pub trial_id: String,
    //pub timestamp: Timestamp,
    pub version: String,
}
