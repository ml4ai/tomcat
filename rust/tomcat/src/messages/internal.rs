use serde::{Deserialize, Serialize};

/// Simplified internal messages containing chat text.
/// These are to be used by the ToMCAT coreference and event extraction teams.
#[derive(Debug, Serialize, Deserialize)]
pub struct InternalChat {
    pub timestamp: String,
    pub sender: String,
    pub text: String,
}

/// Simplified stage transition message to be used by the ToMCAT coreference resolution team.
#[derive(Debug, Serialize, Deserialize)]
pub struct InternalStageTransition {
    pub timestamp: String,
    pub stage: String,
}
