use crate::messages::chat::{Attachment, Extraction};
use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct CompactExtraction {
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub attachments: Vec<Attachment>,
    pub labels: Vec<String>,
    #[serde(skip_serializing)]
    pub span: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub arguments: Option<HashMap<String, Vec<Extraction>>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CompactData {
    pub participant_id: String,
    pub extractions: Vec<CompactExtraction>,
}

/// Compact message to represent natural language understanding system outputs.
#[derive(Debug, Serialize, Deserialize)]
pub struct CompactMessage {
    pub header: Header,
    pub msg: Msg,
    pub data: CompactData,
}

impl CompactMessage {
    fn new(participant_id: String, extractions: Vec<Extraction>) -> Self {
        let mut compact_extractions = vec![];
        for extraction in extractions {
            let compact_extraction = CompactExtraction {
                attachments: extraction.attachments,
                labels: extraction.labels,
                span: extraction.span,
                arguments: extraction.arguments,
            };
        }
        let compact_data = CompactData {
            participant_id,
            extractions: compact_extractions,
        };
        Self {
            data: compact_data,
            msg: Msg {
                trial_id: ""
                experiment_id: ""

            }
        }
    }
}
