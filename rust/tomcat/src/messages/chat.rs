use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};

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
    pub header: Header,
    pub msg: Msg,
    pub data: ChatData,
}
