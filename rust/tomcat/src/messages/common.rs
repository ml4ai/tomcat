use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Header {
    pub timestamp: String,
    pub version: String,
    pub message_type: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Msg {
    pub trial_id: String,
    pub experiment_id: String,
    pub timestamp: String,
    pub source: String,
    pub version: String,
    pub sub_type: String,
    pub replay_parent_id: Option<String>,
    pub replay_id: Option<String>,
    pub replay_parent_type: Option<String>,
}
