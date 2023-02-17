use crate::messages::common::{Header, Msg};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct ClientInfo {
    pub callsign: Option<String>,
    pub markerblocklegend: Option<String>,
    pub participant_id: Option<String>,
    pub playername: Option<String>,
    pub staticmapversion: Option<String>,
    pub unique_id: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TrialData {
    pub client_info: Vec<ClientInfo>,
    pub condition: String,
    pub date: String,
    pub experiment_author: String,
    pub experiment_date: String,
    pub experiment_name: String,
    pub experiment_mission: String,
    pub experimenter: String,
    pub group_number: String,
    pub intervention_agents: Vec<String>,
    pub map_block_filename: String,
    pub map_name: String,
    pub name: String,
    pub notes: Vec<String>,
    pub observers: Vec<String>,
    pub study_number: String,
    pub subjects: Vec<String>,
    pub testbed_version: String,
    pub trial_number: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TrialMessage {
    pub header: Header,
    pub msg: Msg,
    pub data: TrialData,
}
