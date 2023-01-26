use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};

/// Mission stage
#[derive(Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
#[derive(strum_macros::Display)] // Allows variants to be printed as strings if needed
pub enum MissionStage {
    #[default]
    shop_stage,
    field_stage,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StageTransitionData {
    pub mission_timer: String,
    pub elapsed_milliseconds: i64,
    pub mission_stage: MissionStage,
    pub transitionsToShop: i32,
    pub team_budget: i32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct StageTransitionMessage {
    pub header: Header,
    pub msg: Msg,
    pub data: StageTransitionData,
}
