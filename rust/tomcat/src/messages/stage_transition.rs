use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};

/// Mission stage
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
#[derive(strum_macros::Display)] // Allows variants to be printed as strings if needed
pub enum MissionStage {
    field_stage,
    shop_stage,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StageTransitionData {
    pub mission_stage: MissionStage,
    pub transition_number: isize,
    pub max_transitions: isize,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StageTransitionMessage {
    pub header: Header,
    pub msg: Msg,
    pub data: StageTransitionData,
}
