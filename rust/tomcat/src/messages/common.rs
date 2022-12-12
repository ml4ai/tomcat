use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct Header {
    timestamp: String,
    version: String,
    message_type: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Msg {
    trial_id: String,
    experiment_id: String,
    timestamp: String,
    source: String,
    version: String,
    sub_type: String,
    replay_parent_id: Option<String>,
    replay_id: Option<String>,
    replay_parent_type: Option<String>,
}
