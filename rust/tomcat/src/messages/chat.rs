use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use serde_json::Value;

#[derive(Debug, Serialize, Deserialize)]
pub struct ChatData {
    pub text: String,
    pub addressees: Vec<String>,
    pub elapsed_milliseconds: isize,
    pub mission_timer: String,
    pub sender: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ChatMessage {
    header: Header,
    msg: Msg,
    pub data: ChatData
}

// tomcat-text/src/main/scala/org/clulab/asist/attachments/Agent.scala
#[derive(Debug, Serialize, Deserialize)]
pub struct AgentAttachment {
    pub labels: Vec<String>,
    pub agentType: String,
    pub text: String,
    pub span: Vec<u32>
}

// tomcat-text/src/main/scala/org/clulab/asist/attachments/Negation.scala
#[derive(Debug, Serialize, Deserialize)]
pub struct BooleanAttachment {
    pub value: bool,
}

// tomcat-text/src/main/scala/org/clulab/asist/attachments/MarkerId.scala
// tomcat-text/src/main/scala/org/clulab/asist/attachments/Tense.scala
// tomcat-text/src/main/scala/org/clulab/asist/attachments/VictimType.scala
#[derive(Debug, Serialize, Deserialize)]
pub struct StringAttachment {
    pub value: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Attachment {
    Agent(AgentAttachment),
    Bool(BooleanAttachment),
    String(StringAttachment)
}


#[derive(Debug, Serialize, Deserialize)]
pub struct Extraction {
    pub attachments: Vec<Attachment>,
    pub labels: Vec<String>,
    pub span: Option<String>, // field may not exist
    pub arguments: Option<HashMap<String,Vec<Extraction>>>,
    pub start_offset: u32,
    pub end_offset: u32,
    pub rule: Option<String> // field may not exist
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Extractions {
    pub extractions: Vec<Extraction>
//    pub header: Header,
//    pub msg: Msg,
//    pub data: ChatData,
}
