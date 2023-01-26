use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct ClientInfo {
    pub playername: Option<String>,
    pub callsign: Option<String>,
    pub participant_id: Option<String>,
    pub staticmapversion: Option<String>,
    pub markerblocklegend: Option<String>,
    pub unique_id: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TrialData {
    pub name: String,
    pub date: String,
    pub experimenter: String,
    pub subjects: Vec<String>,
    pub trial_number: String,
    pub group_number: String,
    pub study_number: String,
    pub condition: String,
    pub notes: Vec<String>,
    pub testbed_version: String,
    pub experiment_name: String,
    pub experiment_date: String,
    pub experiment_author: String,
    pub experiment_mission: String,
    pub map_name: String,
    pub map_block_filename: String,
    pub intervention_agents: Vec<String>,
    pub client_info: Vec<ClientInfo>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TrialMessage {
    pub header: Header,
    pub msg: Msg,
    pub data: TrialData,
}
