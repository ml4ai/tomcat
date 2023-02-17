use crate::messages::chat::Extraction;
use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct CompactData {
    pub participant_id: String,
    pub extractions: Vec<Extraction>,
}

/// Compact message to represent natural language understanding system outputs.
#[derive(Debug, Serialize, Deserialize)]
pub struct CompactMessage {
    pub header: Header,
    pub msg: Msg,
    pub data: CompactData,
}
