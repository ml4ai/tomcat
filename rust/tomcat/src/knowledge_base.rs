use crate::messages::stage_transition::MissionStage;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct KnowledgeBase {
    /// Mission stage (shop or field)
    pub stage: MissionStage,

    /// Mapping of participant IDs to callsigns
    pub callsign_mapping: HashMap<String, String>,
}
