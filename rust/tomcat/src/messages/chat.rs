use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use serde_json::Value;

#[derive(Debug, Serialize, Deserialize)]
pub struct ChatData {
    pub text: String,
    addressees: Vec<String>,
    elapsed_milliseconds: isize,
    mission_timer: String,
    pub sender: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ChatMessage {
    header: Header,
    msg: Msg,
    pub data: ChatData
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Attachment {
    pub labels: Vec<String>,
    pub agentType: String,
    pub text: String,
    pub span: Vec<u32>
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Extraction {
    pub attachments: Vec<Attachment>,
    pub labels: Vec<String>,
    pub span: String,
    pub arguments: HashMap<String,Extraction>,
    pub start_offset: u32,
    pub end_offset: u32,
    pub rule: String
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Extractions {
    pub extractions: Vec<Extraction>
}
