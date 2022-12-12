use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};

/// Mission stage
#[derive(Debug, Serialize, Deserialize)]
enum PlanningStageState {
    Start,
    Stop,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PlanningStageData {
    mission_timer: String,
    elapsed_milliseconds: isize,
    state: PlanningStageState,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PlanningStageMessage {
    header: Header,
    msg: Msg,
    pub data: PlanningStageData,
}
