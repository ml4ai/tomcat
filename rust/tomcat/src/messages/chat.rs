use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};

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
    pub data: ChatData,
}
