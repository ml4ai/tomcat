use crate::messages::chat::Extraction;
use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct NLUData {
    pub text: String,
    pub participant_id: String,
    pub extractions: Vec<Extraction>,
}

/// Compact message to represent natural language understanding system outputs.
#[derive(Debug, Serialize, Deserialize)]
pub struct NLUMessage {
    pub header: Header,
    pub msg: Msg,
    pub data: NLUData,
}

impl NLUMessage {
    pub fn new(
        text: String,
        participant_id: String,
        extractions: Vec<Extraction>,
        trial_id: String,
        experiment_id: String,
    ) -> Self {
        Self {
            header: Header::new(),
            msg: Msg {
                trial_id: trial_id,
                experiment_id: experiment_id,
                source: "agent".to_string(),
                ..Default::default()
            },
            data: NLUData {
                text,
                participant_id,
                extractions,
            },
        }
    }
}
